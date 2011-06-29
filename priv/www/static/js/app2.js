//version 2, based on knockout

function Table(id, name, columns, template) {
	return {
		id: id,
		name: name,
		columns: columns,
		template: template || 'Не задано',
		select: function () {
			tablesViewModel.current_table(this);
			tablesViewModel.update_table_data();
		},
		remove: function () {
			tablesViewModel.delete_table(this);
		},
		is_selected: function () {
			return tablesViewModel.current_table() && tablesViewModel.current_table().id == this.id;
		},		
	}
}

function Row(id, table_id, data) {
	return {
		id: id,
		table_id: table_id,
		data: ko.observableArray(data),
		remove: function () {
			tablesViewModel.delete_row(this);
		},
		change: function () {
			tablesViewModel.change_row(this);
		},
		print: function () {
			tablesViewModel.print_row(this);
		},
	}
}

function Value(value, type, column_id, column_name) {
	return {
		value: ko.observable(value),
		edit_value: ko.observable(get_presentation(value, type)),
		type: type,
		column_id: column_id,
		column_name: column_name,
		verbose: get_presentation(value, type),
		is_valid: function () {
			var value = this.value();
			if (value != "") {
				if (this.type == "int") return validate_number(value);
			}
			return true;
		},
		get_value: function () {
			var value= $.trim(this.edit_value());
			if (value) {
				if (this.type == 'int') return Number(value);
				else if (this.type == 'datetime') 
					return $.datepicker.parseDate($.datepicker.regional['ru'].dateFormat, value).valueOf();
			}
			this.value(value);
			return value;
		},
	}
}

function Column(id, atom, name, type, has_filter) {
	return {
		id: id,
		atom: ko.observable(atom),
		name: ko.observable(name),
		coltype: ko.observable(type),
		has_filter: ko.observable(has_filter || false),
		remove: function () {
			tablesViewModel.edited_table.columns.remove(this);
		},
		is_valid: function () {
			var col_valid = true;
			
			col_valid = col_valid && validate_atom(this.atom());
			col_valid = col_valid && validate_length(this.name(), 1, 30);
			col_valid = col_valid && validate_choices(this.coltype(), ["int", "datetime", "string"]);
			return col_valid;
		},
	}
}

var tablesViewModel = {
	tables: ko.observableArray([]), //[Table, Table]
	username: ko.observable(""),
	current_table: ko.observable(null), //Table
	rows: ko.observableArray([]), //[Row, Row]
	
	//auth
	auth: {
		username: ko.observable(''),
		password: ko.observable(''),
	},
	
	clear_page: function () {
		this.tables([]);
		this.current_table(null);
		this.rows([]);
		this.username('');
	},
	
	logout: function () {
		var self = this;
		this.action("logout", {}, function () {
			self.clear_page();
			self.check_authorization();
		});
	},
	
	//editing table
	edited_table: {
		name: ko.observable(''),
		template: ko.observable(''),
		columns: ko.observableArray([]), //[Column, Column]
		is_name_valid: function () {
			return validate_length(this.name(), 5, 30)
		},
		is_valid: function () {
			var all_valid = true;
			this.columns().map(function (col) {all_valid = all_valid && col.is_valid(); });
			return all_valid && this.is_name_valid();
		}
	},
	
	edited_row: {
		table_id: 0,
		row_id: 0,
		values: ko.observableArray([]), //[Value, Value]
		save: function () {
			var values = {};
			var self = tablesViewModel;
			for (var i=0; i< this.values().length; i++) {
				var val= this.values()[i];
				values[val.column_id] = val.get_value();
			}
			values['table_id'] = this.table_id;
			
			if (this.row_id) {
				//изменение строки
				values['row_id'] = this.row_id;
				self.action("edit_row", values, function () { self.update_table_data(); });
			}
			else {
				//добавление строки				
				if (values && this.table_id)
					self.action("add_row", values, function () { self.update_table_data();	});
			}
		},
	},
	
	show_auth_dialog: function () { this.auth_dialog.dialog("open"); },
	show_table_dialog: function (cl, title) { 
		this.table_dialog.dialog("option", "dialogClass", cl)
							.dialog("option", "title", title)
							.dialog("open");
	},
	show_editor_dialog: function () { this.editor_dialog.dialog("open"); },
	
	new_table: function () {
		this.edited_table.name('');
		this.edited_table.template('');
		this.edited_table.columns([]);
		this.show_table_dialog("new_table", "Создать таблицу");
	},
	
	edit_table: function () {
		table = this.current_table();
		if (table) {
			this.edited_table.name(table.name);
			this.edited_table.template(table.template);
			columns = [];
			for (key in table.columns) {
				var t = table.columns[key];
				columns.push(new Column(Number(t.id), t.atom, t.name, t.type, t.is_filter));
			}
			this.edited_table.columns(columns);	
			this.show_table_dialog("edit_table", "Изменить таблицу");
		}
	},
	
	save_table: function (table_name, columns, table) {
		var self = this;
		if (table) {
			this.action('edit_table', {'table_name': table_name, 'columns': columns, 'table_id': table.id}, 
					function (data, status, xhr) {
						var options = {
								iframe: true,
								resetForm: true,
								url: "/handle_template?"+$.param({'table_id': data.id}),
						}
						if ($('#file_template').val())
							$('#templateUploader').ajaxSubmit(options);
						self.update_tables_list()
					});
		}
		else {			
			this.action('new_table', {'table_name': table_name, 'columns': columns}, function () {
				self.update_tables_list();
			});
		}
	},
	
	delete_table: function (table) {
		if (confirm("Удалить таблицу??")) {
			var self = this;
			this.action("delete_table", {'table_id': table.id}, function (data, status, xhr) {
				self.update_tables_list();
			});
		}
	},
	
	add_column: function () {
		this.edited_table.columns.push(new Column(0, '', '', 'int'));
	},
	
	//row actions
	
	add_row: function () {
		var table = this.current_table();
		if (table) {
			this.edited_row.row_id = 0;
			this.edited_row.table_id = table.id
			values = [];
			
			for (key in table.columns) {
				var t = table.columns[key];
				values.push(new Value('', t.type, t.id, t.name));
			};
			this.edited_row.values(values);
			this.show_editor_dialog();
			$('input[data-type="datetime"]').datepicker();
		};		
	},
	
	change_row: function (row) {
		var table = this.current_table();
		if (table) {
			this.edited_row.row_id = row.id;
			this.edited_row.table_id = table.id
			
			this.edited_row.values(row.data());
			this.show_editor_dialog();
			$('input[data-type="datetime"]').datepicker();
		};
	},
	
	delete_row: function (row) {
		var self = this;
		this.action("delete_row", {'row_id': row.id}, function (data, status, xhr){
			self.rows.remove(row);
		});
	},
	
	print_row: function (row) {
		var qs = $.param({
			table: row.table_id,
			row: row.id
		})
		location.href="/parse?"+qs;
	},
	
	update_tables_list: function () {
		var self = this;
		this.action('tables_list', {}, function (data, status, xhr) {
			var tables = [];
			
			for (var i=0; i<data.length; i++) tables.push(new Table(data[i].id, data[i].name, data[i].columns, 
															data[i].template));
			self.tables(tables);
			self.parse_hash(window.location.hash.replace(/^#/, '')).check_authorization();
		})
	},
	
	action: function (action_name, values, callback) {
		values.action = action_name;
		var data = JSON.stringify(values);
		var self = this;
		
		function action_callback(data, status, xhr) {
			if (data.error == 'auth_required') self.show_auth_dialog();
			else 
				callback(data, status, xhr);
		}
		$.post('/action', {"json": data}, action_callback, "json");
	},
	
	prepare_row: function (row) {
		var table = this.current_table();
		if (table) {
			var row_data = [];
			for (id in table.columns) {
				var value = row.data[id] || "";
				var coltype = table.columns[id].type;
				var colname = table.columns[id].name;
				row_data.push(new Value(value, coltype, id, colname));
			}
			row.data = row_data;
			return new Row(row.id, row.table_id, row.data);
		}
		return null;
	},
	
	update_table_data: function () {
		var self = this;
		//this.hide_edit_row();
		if (this.current_table())
			this.action("table_rows", {'table_id': this.current_table().id}, function (data, status, xhr) {
				rows = [];
				for (var i=0; i< data.length;i++) {
					var row = self.prepare_row(data[i]);
					if (row) rows.push(row);
				}
				self.rows(rows);
			})
	},
	
	init: function () {
		//this.clear_page();
		this.update_tables_list();
		this.init_dialogs();
	},
	
	parse_hash: function (hash) {
		try {
			var params = eval('('+hash+')');
		}
		catch (e) {
			if (hash != "") location.href = '/';
			return this; 
		};
		if (params.table) {
			for (var i=0; i<this.tables().length; i++)
				if (params.table == this.tables()[i].id) {
					this.tables()[i].select();
					break;
				}
		}
		return this;
	},
	
	check_authorization: function () {
		var self = this;
		this.action("auth_info", {}, function (info, status, xhr) {	self.username(info.username); });	
	},
	
	init_dialogs: function () {
		var self = this;
		this.table_dialog = $('#table-edit-dialog').dialog({ 
			autoOpen: false, 
			modal: true,
			height: 600,
			width: 600,
			title: 'Создание/изменение таблицы',
			buttons: {
				"Сохранить": function() {
					var columns = [];
					self.edited_table.columns().map(function (col) {
						if (col.is_valid()) columns.push([col.name(), col.coltype(), col.has_filter(), col.id, col.atom()]);
					});
					if (self.edited_table.is_valid() && columns.length) {
						var cl = $(this).dialog( "option", "dialogClass" );
						if (cl == 'new_table')
							self.save_table(self.edited_table.name(), columns);
						else if (cl == 'edit_table')
							self.save_table(self.edited_table.name(), columns, self.current_table());
						$( this ).dialog( "close" );
					};
				},
				"Отмена": function() {
					$( this ).dialog( "close" );
				}
			},
		});
		
		this.auth_dialog = $('#auth-dialog').dialog({ 
			autoOpen: false, 
			modal: true,
			height: 200,
			width: 480,
			title: 'Авторизация',
			buttons: {
				"Попробовать": function() {					
					self.action("authenticate", {'username': self.auth.username(), 'password': self.auth.password()},
						function (data, status, xhr) {
							$()
							self.update_tables_list();
						});
					$( this ).dialog( "close" );
				},
				"Отмена": function() {	$( this ).dialog( "close" ); }
			},
		});
		this.editor_dialog = $('#row-editor-dialog').dialog({ 
			autoOpen: false, 
			modal: true,
			height: 400,
			width: 480,
			title: 'Добавление/изменение строки',
			buttons: {
				"Сохранить": function() {tablesViewModel.edited_row.save(); $( this ).dialog( "close" ); },
				"Отмена": function() {	$( this ).dialog( "close" ); }
			},
		});
	}
}

tablesViewModel.current_columns = ko.dependentObservable(function () {
	if (this.current_table() != null) return this.current_table().columns;
	else return [];
}, tablesViewModel);

$(function () {
	ko.applyBindings(tablesViewModel);
	tablesViewModel.init();
	
	$.history.init(function(hash){
		tablesViewModel.parse_hash(hash);
    }, { unescape: ",/" });
});

//presentation of values

function get_presentation(value, type) {
	var val = $.trim(value);
	if (val == "") return val;
	
	try {
		if (type == "int") {
			val = Number(val);
			if (isNaN(val)) throw "not number";
		}
		if (type == "datetime") {
			var ms = Number(val);
			if (isNaN(ms)) throw "not number";
			date = new Date(ms);
			val = $.datepicker.formatDate($.datepicker.regional['ru'].dateFormat, date);
		}
		return val;
	}
	catch (e) {return "#err"};
}

//validators

function validate_number(value) {
	return /^-?(?:\d+|\d{1,3}(?:,\d{3})+)(?:\.\d+)?$/.test(value);
}

function validate_atom(value) {
	return /^[a-z_]+$/.test(value);
}

function validate_length(val, min, max ) {
	return !( val.length > max || val.length < min );
}

function checkNumber(o, n) {
	value = o.val();
	if (value) {
		result = validate_number(value);
		return mark_by_result(o, result);
	}
	else return true;
}

function validate_choices(val, values) {
	for (var i=0; i<values.length; i++)
		if (values[i] === val) return true;
	return false;
}