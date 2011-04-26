function TablesEditor() {
	this.init();
	var self = this;
	this.table_dialog = $('#table-edit-dialog').dialog({ 
		autoOpen: false, 
		modal: true,
		height: 400,
		width: 480,
		title: 'Создание/изменение таблицы',
		buttons: {
			"Сохранить": function() {
				var name = $("#table_name");
				var bValid = true;
				
				bValid = bValid && checkLength( name, "Название таблицы", 5, 30);
				
				var columns = [];
				$('table.columns tbody tr').each(function (idx, item) {
					var col_valid = true;
					var col_id = Number($(item).attr('data-column'));
					
					var wname = $(item).find('input[type="text"]');
					var wtype = $(item).find('select');
					var wfilter = $(item).find('input[type="checkbox"]');
					
					var col_name = wname.val();
					col_valid = col_valid && checkLength(wname, "Имя столбца "+idx, 1, 30);
					var type = wtype.val();
					col_valid = col_valid && checkChoices(wtype, "Тип столбца "+idx, ["int", "datetime", "string"]);
					var has_filter = wfilter.attr('checked');
					
					if (col_valid) columns.push([col_name, type, has_filter, col_id]);
				});
				if ( bValid && columns.length) {
					var cl = $(this).dialog( "option", "dialogClass" );
					if (cl == 'new_table')
						self.new_table(name.val(), columns);
					else if (cl == 'edit_table')
						self.edit_table(self.current_table, name.val(), columns);
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
				var username = $("#username");
				var password = $("#password");
				
				var bValid = true;				
				bValid = bValid && checkLength( username, "Имя пользователя", 5, 30);
				bValid = bValid && checkLength( password, "Пароль", 1, 200);
				
				if (bValid) {
					self.action("authenticate", {'username': username.val(), 'password': password.val()},
							function (data, status, xhr) {
								self.update_tables_list();
							});
					$( this ).dialog( "close" );
				}
			},
			"Отмена": function() {
				$( this ).dialog( "close" );
			}
		},
	});
}

var ONE_ROW_DIRECTIVES =  {
	'.@data-row': 'row.id',
	'td.presentation': {
		'ob<-row.data': {
			'.': function (obj) {return get_presentation(obj.item.value, obj.item.type);}
		}
	},
};

TablesEditor.prototype = {
	list_directives: {
		'li': {
			'table<-tables': {
				'a@href': '#{table:#{table.id}}',
				'a': 'table.name',
				'a@data-table': 'table.id',
				'button@data-table': 'table.id',
			},
		}
	},
	
	head_directives: {
		'th': {
			'column<-columns': {
				'.': 'column.name',				
			}
		}
	},
	
	edit_directives: {
		'td.inputs': {
			'column<-columns': {
				'input@data-column': 'column.id',
				'input@data-type': 'column.type',
				'input@value': function (a) {
					var row = a.context.row;
					var column = a.item;
					if (row) {
						for (var i=0; i<row.data.length; i++) {
							if (row.data[i].column_id == column.id)
								return get_presentation(row.data[i].value, row.data[i].type);
						}
					}
					return '';
				}
			}
		}
	},
	
	one_row_directives: ONE_ROW_DIRECTIVES,
	
	presentation_directives: {
		'tr': {
			'row<-rows': ONE_ROW_DIRECTIVES,
		}
	},
	
	column_directives: {
		'tr': {
			'column<-columns': {
				'.@data-column': 'column.id',
				'input[type="text"]@value': 'column.name',
				'input[type="checkbox"]@checked': 'column.is_filter',
			}
		}
	},
	
	tmpl_table_list: false,
	tables: {},
	tmpl_column: null,
	tmpl_table_head: null,
	tmpl_row_edit: null,
	tmpl_row_presentation: null,
	tmpl_one_row: null,
	tmpl_auth_block: null,
	tmpl_login: '<a href="javascript:void(0);" class="login"> Войти в систему </a>',
	
	error_value: '#err',
	rows: {},
	
	show_error: function (msg) {
		alert(msg);
		return false;
	},
	
	init: function () {
		this.tmpl_column = this.tmpl_column || $('table.columns tbody').html();
		var tbr = $('.table_view tbody');
		this.tmpl_row_presentation = this.tmpl_row_presentation || tbr.html();
		this.tmpl_one_row = this.tmpl_one_row || tbr.find('tr').html();
		
		this.tmpl_auth_block = this.tmpl_auth_block || $('header .info').html();
		this.tmpl_table_head = this.tmpl_table_head || $('.table_view thead tr').html();
		this.tmpl_table_list = this.tmpl_table_list || $('.tables_list').html();
		this.tmpl_row_edit = this.tmpl_row_edit || $('.table_view tfoot tr').html();
		
		this.current_table = 0;		
		this.clear_page();
		this.update_tables_list();
	},
	
	clear_page: function () {
		$('header .info').empty().html(this.tmpl_login);		
		$('.table_view tbody').empty();
		$('.tables_list').empty();
		
		$('.table_content').find('.text').show().end().find('.table_view').hide();
		this.clear_dialogs();
	},
	
	clear_dialogs: function () {
		$('#table_name').val("");
		$('table.columns tbody').empty();
	},
	
	check_authorization: function () {
		var self = this;
		this.action("auth_info", {}, function (data, status, xhr) {
			if (data.username) {
				$('header .info').html(self.tmpl_auth_block).autoRender(data);
			}
		});		
	},
	
	logout: function () {
		var self = this;
		this.action("logout", {}, function () {
			self.clear_page();
			self.check_authorization();
		});
	},
	
	show_auth_dialog: function () {
		this.auth_dialog.dialog("open");
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
	
	new_table: function (table_name, columns) {
		var self = this;
		this.action('new_table', {'table_name': table_name, 'columns': columns}, function () {
			self.update_tables_list();
		});
	},
	
	edit_table: function (table_id, table_name, columns) {		
		var self = this;
		this.action('edit_table', {'table_name': table_name, 'columns': columns, 'table_id': table_id}, 
				function () {
					self.update_tables_list();
				});
	},
	
	render: function (selector, template, data, directives) {
		$(selector).empty().html(template).render(data, directives).show();
		return this;
	},
	
	fill_change_dialog: function () {
		var table = this.tables[this.current_table];
		if (table) {
			$('#table_name').val(table.name);
			
			var tbody_selector = 'table.columns tbody';
			this.render(tbody_selector, this.tmpl_column, {'columns': table.columns}, this.column_directives);
			for (id in table.columns) {
				var col = table.columns[id];
				var selector = tbody_selector+' tr[data-column="'+id+'"] select';				
				$(selector).val(col.type);
			};
		}
		return table;
	},
	
	select_table: function (table_id) {
		$('.tables_list a').removeClass("selected").end().find('a[data-table="'+table_id+'"]').addClass('selected');
		this.current_table = table_id-0;
		
		var table = this.tables[this.current_table];
		
		if (table) {
			$('.table_content').find('.text').hide().end().find('.table_view').show();
			this.render('.table_view thead tr', this.tmpl_table_head, {'columns': table.columns}, this.head_directives)
				.update_table_data();
		}
	},
	
	show_edit_row: function () {
		var self = this;
		var table = this.tables[this.current_table];
		if (table) {
			var selector = this.dom('table_footer');
			this.render(selector, this.tmpl_row_edit, {'columns': table.columns}, this.edit_directives);
			var tf = $(selector);
			tf.find('input[data-type="datetime"]').datepicker();
			
			//actions
			tf.find('button.add').click(function (e) {
				var row_data = self.collect_input_data(tf);
				var table = self.tables[self.current_table];
				
				row_data['table_id'] = table.id;
				if (row_data && table)
					self.action("add_row", row_data, function () {
						self.update_table_data();
					});
			});
			tf.find('button.hide').click(this.hide_edit_row);
		}
	},
	
	dom: function (d) {
		if (d.row) return '.table_view tr[data-row="'+d.row+'"]';
		else if (d == 'table_header') 
			return '.table_view thead tr';
		else if (d == 'table_footer') 
			return '.table_view tfoot tr';
		else if (d == 'table_body')
			return '.table_view tbody';
 	},
	
	show_change_row: function (row_id) {
		var self = this;
		var row = this.rows[row_id];
		
		var table = this.tables[this.current_table];
		if (table) {
			var selector = this.dom({'row': row_id});
			self.render(selector, self.tmpl_row_edit, 
					{'columns': table.columns, 'row': row}, self.edit_directives);
			
			var tf = $(selector);
			tf.find('input[data-type="datetime"]').datepicker();
			tf.find('button.add').click(function (e) {
				var row_data = self.collect_input_data(tf);
				var table = self.tables[self.current_table];
				
				row_data['table_id'] = table.id;
				row_data['row_id'] = row_id;
				if (row_data && table)
					self.action("edit_row", row_data, function () {
						self.update_table_data();
					});
			});
			tf.find('button.hide').click(function (e) {
				self.render(selector, self.tmpl_one_row, {'row': row}, self.one_row_directives);
			});
		}
	},
	
	delete_row: function (row_id) {
		var self = this;
		this.action("delete_row", {'row_id': row_id}, function (data, status, xhr){
			$(self.dom({'row': row_id})).remove();
		});
	},
	
	collect_input_data: function (line) {
		var inputs = line.find('input');
		bValid = true;
		var values = {};
		for (var i=0; i<inputs.length; i++) {
			var inp = $(inputs[i]);
			var type = inp.attr('data-type');
			var id = inp.attr('data-column');
			var value = $.trim(inp.val());
			if (value != "") {
				if (type == "int") {
					bValid = bValid && checkNumber(inp);
					value = Number(value);
				}
				else if (type == "datetime") {
					value = inp.datepicker("getDate").valueOf();
				}
			}
			values[id-0] = value;
			
		}
		if (bValid) return values; else return null;
	},
	
	hide_edit_row: function () {
		var tf = $('.table_view tfoot tr');
		tf.hide();
	},
	
	delete_table: function (table_id) {
		var self = this;
		this.action("delete_table", {'table_id': table_id}, function (data, status, xhr) {
			self.update_tables_list();
		});
	},
	
	update_tables_list: function () {
		var self = this;
		this.action('tables_list', {}, function (data, status, xhr) {
			self.tables = {};
			for (var i=0; i<data.length; i++) 
				self.tables[data[i].id] = data[i];
			self.render('.tables_list', self.tmpl_table_list, {'tables': data}, self.list_directives)
				.parse_hash(window.location.hash.replace(/^#/, ''))
				.check_authorization();
		})
	},
	
	prepare_row: function (row) {
		var table = this.tables[row.table_id];
		if (table) {
			var row_data = [];
			for (id in table.columns) {
				var value = row.data[id] || "";
				var coltype = table.columns[id].type;				
				row_data.push({'value': value, 'type': coltype, 'column_id': id});
			}
			row.data = row_data;
			return row;
		}
		return null;
	},
	
	update_table_data: function () {
		var self = this;
		this.hide_edit_row();
		if (this.current_table)
			this.action("table_rows", {'table_id': this.current_table}, function (data, status, xhr) {
				self.rows = {};
				for (var i=0; i< data.length;i++) {
					row = self.prepare_row(data[i]);
					if (row)
						self.rows[row.id] = row;
				}
				self.render(self.dom('table_body'), self.tmpl_row_presentation, 
						{'rows': self.rows}, self.presentation_directives);
			})
	},
	
	parse_hash: function (hash) {
		try {
			var params = eval('('+hash+')');
		}
		catch (e) {
			if (hash != "") location.href = '/';
			return false; 
		};
		if (params.table) {
			this.select_table(params.table);
		}
		return this;
	}
}

var tables_editor = null;

$(function () {
	tables_editor = new TablesEditor();
	$("table.columns tbody" ).sortable();
	
	$.history.init(function(hash){
        tables_editor.parse_hash(hash);
    }, { unescape: ",/" });
	
	$('.action.new_table').click(function (e) {
		tables_editor.clear_dialogs();
		tables_editor.table_dialog
			.dialog("option", "dialogClass", 'new_table')
			.dialog("option", "title", 'Создать таблицу' )
			.dialog("open");
		//$("table.columns tbody" ).sortable();
		return false;
	});
	
	$('.action.edit_table').click(function (e) {
		tables_editor.clear_dialogs();
		table = tables_editor.fill_change_dialog();
		if (table) {
			tables_editor.table_dialog
				.dialog("option", "dialogClass", 'edit_table')
				.dialog("option", "title", 'Изменить таблицу' )
				.dialog("open");
			$("table.columns tbody" ).sortable();
		}
		return false;
	});
	
	$('.action.add_row').click(function (e) {
		tables_editor.show_edit_row();
		return false;
	});
	
	$('button.delcol').live('click', function (e) {
		e.preventDefault();
		$(this).parents('tr').eq(0).remove();
		return false;
	});
	
	$('button.addcol').live('click', function (e) {
		$('table.columns tbody').append(tables_editor.tmpl_column);
	});
	
	$('button.change_row').live('click', function (e) {
		var row_id = Number($(this).parents('tr').eq(0).attr('data-row'));
		if (!isNaN(row_id))
			tables_editor.show_change_row(row_id);
	});
	
	$('button.delete_row').live('click', function (e) {
		if (confirm("Точно удалить строку?")) {
			var row_id = Number($(this).parents('tr').eq(0).attr('data-row'));
			if (!isNaN(row_id))
				tables_editor.delete_row(row_id);
		}
	});
	
	$('button.deltable').live('click', function (e) {
		if (confirm("Удалить таблицу??")) {
			if (confirm("Точно удалить таблицу?? Данные будет потеряны безвозвратно"))
				tables_editor.delete_table($(this).attr('data-table')-0);
		}
	});
	
	$('.logout').live('click', function (e) {
		tables_editor.logout();
	});
	
	$('.login').live('click', function (e) {
		tables_editor.show_auth_dialog();
	});
});

//validators

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
	catch (e) {return tables_editor.error_value};
}

function validate_number(value) {
	return /^-?(?:\d+|\d{1,3}(?:,\d{3})+)(?:\.\d+)?$/.test(value);
}

function mark_by_result(o, result) {
	if (!result) o.addClass( "ui-state-error" ); else o.removeClass("ui-state-error");
	return result;
}

function checkLength( o, n, min, max ) {
	if ( o.val().length > max || o.val().length < min ) return mark_by_result(o, false);
	else return mark_by_result(o, true);
}

function checkNumber(o, n) {
	value = o.val();
	if (value) {
		result = validate_number(value);
		return mark_by_result(o, result);
	}
	else return true;
}

function checkRegexp( o, regexp, n ) {
	if ( !( regexp.test( o.val() ) ) ) {
		o.addClass( "ui-state-error" );
		return false;
	} else {
		return true;
	}
}

function checkChoices(o, n, values) {
	var val = o.val();
	for (var i=0; i<values.length; i++)
		if (values[i] === val) return true;
	return false;
}