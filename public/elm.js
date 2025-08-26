(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$document = _Browser_document;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Model$Map = F4(
	function (id, items, rect, parentMapId) {
		return {id: id, items: items, parentMapId: parentMapId, rect: rect};
	});
var $author$project$Model$NoEdit = {$: 'NoEdit'};
var $author$project$Model$Rectangle = F4(
	function (x1, y1, x2, y2) {
		return {x1: x1, x2: x2, y1: y1, y2: y2};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$IconMenu$init = {open: false};
var $author$project$Mouse$NoDrag = {$: 'NoDrag'};
var $author$project$Mouse$init = {dragState: $author$project$Mouse$NoDrag};
var $author$project$Search$Closed = {$: 'Closed'};
var $author$project$Search$init = {menu: $author$project$Search$Closed, result: _List_Nil, text: ''};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $author$project$AppModel$default = {
	editState: $author$project$Model$NoEdit,
	iconMenu: $author$project$IconMenu$init,
	items: $elm$core$Dict$empty,
	mapPath: _List_fromArray(
		[0]),
	maps: A2(
		$elm$core$Dict$singleton,
		0,
		A4(
			$author$project$Model$Map,
			0,
			$elm$core$Dict$empty,
			A4($author$project$Model$Rectangle, 0, 0, 0, 0),
			-1)),
	measureText: '',
	mouse: $author$project$Mouse$init,
	nextId: 1,
	search: $author$project$Search$init,
	selection: _List_Nil
};
var $elm$core$Debug$log = _Debug_log;
var $author$project$Utils$info = F2(
	function (funcName, val) {
		return A2($elm$core$Debug$log, '@' + funcName, val);
	});
var $author$project$Utils$logError = F3(
	function (funcName, text, val) {
		return A2($elm$core$Debug$log, '### ERROR @' + (funcName + (': ' + text)), val);
	});
var $author$project$Model$Assoc = function (a) {
	return {$: 'Assoc', a: a};
};
var $author$project$Model$AssocInfo = F6(
	function (id, itemType, player1, role1, player2, role2) {
		return {id: id, itemType: itemType, player1: player1, player2: player2, role1: role1, role2: role2};
	});
var $author$project$AppModel$Model = function (items) {
	return function (maps) {
		return function (mapPath) {
			return function (nextId) {
				return function (selection) {
					return function (editState) {
						return function (measureText) {
							return function (mouse) {
								return function (search) {
									return function (iconMenu) {
										return {editState: editState, iconMenu: iconMenu, items: items, mapPath: mapPath, maps: maps, measureText: measureText, mouse: mouse, nextId: nextId, search: search, selection: selection};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Model$Topic = function (a) {
	return {$: 'Topic', a: a};
};
var $author$project$Model$TopicInfo = F3(
	function (id, text, iconName) {
		return {iconName: iconName, id: id, text: text};
	});
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded = A2($elm$core$Basics$composeR, $elm$json$Json$Decode$succeed, $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom);
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Model$AssocProps = {};
var $author$project$Model$MapAssoc = function (a) {
	return {$: 'MapAssoc', a: a};
};
var $author$project$Model$MapItem = F5(
	function (id, hidden, pinned, props, parentAssocId) {
		return {hidden: hidden, id: id, parentAssocId: parentAssocId, pinned: pinned, props: props};
	});
var $author$project$Model$MapTopic = function (a) {
	return {$: 'MapTopic', a: a};
};
var $author$project$Model$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $author$project$Model$Size = F2(
	function (w, h) {
		return {h: h, w: w};
	});
var $author$project$Model$TopicProps = F3(
	function (pos, size, displayMode) {
		return {displayMode: displayMode, pos: pos, size: size};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Model$BlackBox = {$: 'BlackBox'};
var $author$project$Model$Container = function (a) {
	return {$: 'Container', a: a};
};
var $author$project$Model$Detail = {$: 'Detail'};
var $author$project$Model$LabelOnly = {$: 'LabelOnly'};
var $author$project$Model$Monad = function (a) {
	return {$: 'Monad', a: a};
};
var $author$project$Model$Unboxed = {$: 'Unboxed'};
var $author$project$Model$WhiteBox = {$: 'WhiteBox'};
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Storage$displayModeDecoder = function (str) {
	switch (str) {
		case 'LabelOnly':
			return $elm$json$Json$Decode$succeed(
				$author$project$Model$Monad($author$project$Model$LabelOnly));
		case 'Detail':
			return $elm$json$Json$Decode$succeed(
				$author$project$Model$Monad($author$project$Model$Detail));
		case 'BlackBox':
			return $elm$json$Json$Decode$succeed(
				$author$project$Model$Container($author$project$Model$BlackBox));
		case 'WhiteBox':
			return $elm$json$Json$Decode$succeed(
				$author$project$Model$Container($author$project$Model$WhiteBox));
		case 'Unboxed':
			return $elm$json$Json$Decode$succeed(
				$author$project$Model$Container($author$project$Model$Unboxed));
		default:
			return $elm$json$Json$Decode$fail('\"' + (str + '\" is an invalid display mode'));
	}
};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $author$project$Storage$strToIntDict = function (strDict) {
	return A3(
		$elm$core$Dict$foldl,
		F3(
			function (k, v, b) {
				if (b.$ === 'Just') {
					var b_ = b.a;
					var _v1 = $elm$core$String$toInt(k);
					if (_v1.$ === 'Just') {
						var i = _v1.a;
						return $elm$core$Maybe$Just(
							A3($elm$core$Dict$insert, i, v, b_));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}),
		$elm$core$Maybe$Just($elm$core$Dict$empty),
		strDict);
};
var $author$project$Storage$strToIntDictDecoder = function (strDict) {
	var _v0 = $author$project$Storage$strToIntDict(strDict);
	if (_v0.$ === 'Just') {
		var dict = _v0.a;
		return $elm$json$Json$Decode$succeed(dict);
	} else {
		return $elm$json$Json$Decode$fail('Transformation Dict String -> Int failed');
	}
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Storage$mapDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Model$Map,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'items',
		A2(
			$elm$json$Json$Decode$andThen,
			$author$project$Storage$strToIntDictDecoder,
			$elm$json$Json$Decode$dict(
				A6(
					$elm$json$Json$Decode$map5,
					$author$project$Model$MapItem,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'hidden', $elm$json$Json$Decode$bool),
					A2($elm$json$Json$Decode$field, 'pinned', $elm$json$Json$Decode$bool),
					$elm$json$Json$Decode$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$json$Json$Decode$field,
								'topicProps',
								A2(
									$elm$json$Json$Decode$map,
									$author$project$Model$MapTopic,
									A4(
										$elm$json$Json$Decode$map3,
										$author$project$Model$TopicProps,
										A2(
											$elm$json$Json$Decode$field,
											'pos',
											A3(
												$elm$json$Json$Decode$map2,
												$author$project$Model$Point,
												A2($elm$json$Json$Decode$field, 'x', $elm$json$Json$Decode$float),
												A2($elm$json$Json$Decode$field, 'y', $elm$json$Json$Decode$float))),
										A2(
											$elm$json$Json$Decode$field,
											'size',
											A3(
												$elm$json$Json$Decode$map2,
												$author$project$Model$Size,
												A2($elm$json$Json$Decode$field, 'w', $elm$json$Json$Decode$float),
												A2($elm$json$Json$Decode$field, 'h', $elm$json$Json$Decode$float))),
										A2(
											$elm$json$Json$Decode$andThen,
											$author$project$Storage$displayModeDecoder,
											A2($elm$json$Json$Decode$field, 'displayMode', $elm$json$Json$Decode$string))))),
								A2(
								$elm$json$Json$Decode$field,
								'assocProps',
								$elm$json$Json$Decode$succeed(
									$author$project$Model$MapAssoc($author$project$Model$AssocProps)))
							])),
					A2($elm$json$Json$Decode$field, 'parentAssocId', $elm$json$Json$Decode$int))))),
	A2(
		$elm$json$Json$Decode$field,
		'rect',
		A5(
			$elm$json$Json$Decode$map4,
			$author$project$Model$Rectangle,
			A2($elm$json$Json$Decode$field, 'x1', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'y1', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'x2', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'y2', $elm$json$Json$Decode$float))),
	A2($elm$json$Json$Decode$field, 'parentMapId', $elm$json$Json$Decode$int));
var $author$project$Storage$maybeString = function (str) {
	return $elm$json$Json$Decode$succeed(
		function () {
			if (str === '') {
				return $elm$core$Maybe$Nothing;
			} else {
				return $elm$core$Maybe$Just(str);
			}
		}());
};
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $author$project$Storage$modelDecoder = A2(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
	$author$project$AppModel$default.iconMenu,
	A2(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
		$author$project$AppModel$default.search,
		A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
			$author$project$AppModel$default.mouse,
			A2(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
				$author$project$AppModel$default.measureText,
				A2(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
					$author$project$AppModel$default.editState,
					A2(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$hardcoded,
						$author$project$AppModel$default.selection,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'nextId',
							$elm$json$Json$Decode$int,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'mapPath',
								$elm$json$Json$Decode$list($elm$json$Json$Decode$int),
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'maps',
									A2(
										$elm$json$Json$Decode$andThen,
										$author$project$Storage$strToIntDictDecoder,
										$elm$json$Json$Decode$dict($author$project$Storage$mapDecoder)),
									A3(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'items',
										A2(
											$elm$json$Json$Decode$andThen,
											$author$project$Storage$strToIntDictDecoder,
											$elm$json$Json$Decode$dict(
												$elm$json$Json$Decode$oneOf(
													_List_fromArray(
														[
															A2(
															$elm$json$Json$Decode$field,
															'topic',
															A2(
																$elm$json$Json$Decode$map,
																$author$project$Model$Topic,
																A4(
																	$elm$json$Json$Decode$map3,
																	$author$project$Model$TopicInfo,
																	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
																	A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string),
																	A2(
																		$elm$json$Json$Decode$andThen,
																		$author$project$Storage$maybeString,
																		A2($elm$json$Json$Decode$field, 'iconName', $elm$json$Json$Decode$string))))),
															A2(
															$elm$json$Json$Decode$field,
															'assoc',
															A2(
																$elm$json$Json$Decode$map,
																$author$project$Model$Assoc,
																A7(
																	$elm$json$Json$Decode$map6,
																	$author$project$Model$AssocInfo,
																	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
																	A2($elm$json$Json$Decode$field, 'itemType', $elm$json$Json$Decode$string),
																	A2($elm$json$Json$Decode$field, 'player1', $elm$json$Json$Decode$int),
																	A2($elm$json$Json$Decode$field, 'role1', $elm$json$Json$Decode$string),
																	A2($elm$json$Json$Decode$field, 'player2', $elm$json$Json$Decode$int),
																	A2($elm$json$Json$Decode$field, 'role2', $elm$json$Json$Decode$string))))
														])))),
										$elm$json$Json$Decode$succeed($author$project$AppModel$Model)))))))))));
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Utils$toString = $elm$core$Debug$toString;
var $author$project$Main$init = function (flags) {
	return _Utils_Tuple2(
		function () {
			var _v0 = A2(
				$elm$json$Json$Decode$decodeValue,
				$elm$json$Json$Decode$null(true),
				flags);
			if ((_v0.$ === 'Ok') && _v0.a) {
				var _v1 = A2($author$project$Utils$info, 'init', 'localStorage: empty');
				return $author$project$AppModel$default;
			} else {
				var _v2 = A2($elm$json$Json$Decode$decodeValue, $author$project$Storage$modelDecoder, flags);
				if (_v2.$ === 'Ok') {
					var model = _v2.a;
					var _v3 = A2(
						$author$project$Utils$info,
						'init',
						'localStorage: ' + ($elm$core$String$fromInt(
							$elm$core$String$length(
								$author$project$Utils$toString(model))) + ' bytes'));
					return model;
				} else {
					var e = _v2.a;
					var _v4 = A3($author$project$Utils$logError, 'init', 'localStorage', e);
					return $author$project$AppModel$default;
				}
			}
		}(),
		$elm$core$Platform$Cmd$none);
};
var $author$project$AppModel$Mouse = function (a) {
	return {$: 'Mouse', a: a};
};
var $author$project$Mouse$Move = function (a) {
	return {$: 'Move', a: a};
};
var $author$project$Mouse$Up = {$: 'Up'};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $author$project$MouseAPI$dragSub = $elm$core$Platform$Sub$batch(
	_List_fromArray(
		[
			$elm$browser$Browser$Events$onMouseMove(
			A2(
				$elm$json$Json$Decode$map,
				$author$project$AppModel$Mouse,
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Mouse$Move,
					A3(
						$elm$json$Json$Decode$map2,
						$author$project$Model$Point,
						A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
						A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float))))),
			$elm$browser$Browser$Events$onMouseUp(
			A2(
				$elm$json$Json$Decode$map,
				$author$project$AppModel$Mouse,
				$elm$json$Json$Decode$succeed($author$project$Mouse$Up)))
		]));
var $author$project$Mouse$Down = {$: 'Down'};
var $author$project$Mouse$DownItem = F4(
	function (a, b, c, d) {
		return {$: 'DownItem', a: a, b: b, c: c, d: d};
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$browser$Browser$Events$onMouseDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousedown');
var $author$project$Utils$strToIntDecoder = function (str) {
	var _v0 = $elm$core$String$toInt(str);
	if (_v0.$ === 'Just') {
		var _int = _v0.a;
		return $elm$json$Json$Decode$succeed(_int);
	} else {
		return $elm$json$Json$Decode$fail('\"' + (str + '\" is an invalid ID'));
	}
};
var $author$project$MouseAPI$mouseDownSub = $elm$browser$Browser$Events$onMouseDown(
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$map,
				$author$project$AppModel$Mouse,
				A5(
					$elm$json$Json$Decode$map4,
					$author$project$Mouse$DownItem,
					$elm$json$Json$Decode$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$json$Json$Decode$at,
								_List_fromArray(
									['target', 'className']),
								$elm$json$Json$Decode$string),
								A2(
								$elm$json$Json$Decode$at,
								_List_fromArray(
									['target', 'className', 'baseVal']),
								$elm$json$Json$Decode$string)
							])),
					A2(
						$elm$json$Json$Decode$andThen,
						$author$project$Utils$strToIntDecoder,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['target', 'dataset', 'id']),
							$elm$json$Json$Decode$string)),
					A2(
						$elm$json$Json$Decode$andThen,
						$author$project$Utils$strToIntDecoder,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['target', 'dataset', 'mapId']),
							$elm$json$Json$Decode$string)),
					A3(
						$elm$json$Json$Decode$map2,
						$author$project$Model$Point,
						A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
						A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float)))),
				$elm$json$Json$Decode$succeed(
				$author$project$AppModel$Mouse($author$project$Mouse$Down))
			])));
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$MouseAPI$mouseSubs = function (model) {
	var _v0 = model.mouse.dragState;
	switch (_v0.$) {
		case 'WaitForStartTime':
			return $elm$core$Platform$Sub$none;
		case 'WaitForEndTime':
			return $elm$core$Platform$Sub$none;
		case 'DragEngaged':
			return $author$project$MouseAPI$dragSub;
		case 'Drag':
			return $author$project$MouseAPI$dragSub;
		default:
			return $author$project$MouseAPI$mouseDownSub;
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ModelAPI$activeMap = function (model) {
	var _v0 = $elm$core$List$head(model.mapPath);
	if (_v0.$ === 'Just') {
		var mapId = _v0.a;
		return mapId;
	} else {
		return A3($author$project$Utils$logError, 'activeMap', 'mapPath is empty!', 0);
	}
};
var $author$project$ModelAPI$nextId = function (model) {
	return _Utils_update(
		model,
		{nextId: model.nextId + 1});
};
var $author$project$ModelAPI$createAssoc = F6(
	function (itemType, player1, role1, player2, role2, model) {
		var id = model.nextId;
		var assoc = A6($author$project$Model$AssocInfo, id, itemType, player1, role1, player2, role2);
		return _Utils_Tuple2(
			$author$project$ModelAPI$nextId(
				_Utils_update(
					model,
					{
						items: A3(
							$elm$core$Dict$insert,
							id,
							$author$project$Model$Assoc(assoc),
							model.items)
					})),
			id);
	});
var $author$project$ModelAPI$illegalId = F4(
	function (funcName, item, id, val) {
		return A3(
			$author$project$Utils$logError,
			funcName,
			$elm$core$String$fromInt(id) + (' is an illegal ' + (item + ' ID')),
			val);
	});
var $author$project$ModelAPI$illegalMapId = F3(
	function (funcName, id, val) {
		return A4($author$project$ModelAPI$illegalId, funcName, 'Map', id, val);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$ModelAPI$updateMaps = F3(
	function (mapId, mapFunc, maps) {
		return A3(
			$elm$core$Dict$update,
			mapId,
			function (map_) {
				if (map_.$ === 'Just') {
					var map = map_.a;
					return $elm$core$Maybe$Just(
						mapFunc(map));
				} else {
					return A3($author$project$ModelAPI$illegalMapId, 'updateMaps', mapId, $elm$core$Maybe$Nothing);
				}
			},
			maps);
	});
var $author$project$ModelAPI$addItemToMap = F4(
	function (itemId, props, mapId, model) {
		var _v0 = A6($author$project$ModelAPI$createAssoc, 'dmx.composition', itemId, 'dmx.child', mapId, 'dmx.parent', model);
		var newModel = _v0.a;
		var parentAssocId = _v0.b;
		var mapItem = A5($author$project$Model$MapItem, itemId, false, false, props, parentAssocId);
		var _v1 = A2(
			$author$project$Utils$info,
			'addItemToMap',
			{itemId: itemId, mapId: mapId, parentAssocId: parentAssocId, props: props});
		return _Utils_update(
			newModel,
			{
				maps: A3(
					$author$project$ModelAPI$updateMaps,
					mapId,
					function (map) {
						return _Utils_update(
							map,
							{
								items: A3($elm$core$Dict$insert, itemId, mapItem, map.items)
							});
					},
					newModel.maps)
			});
	});
var $author$project$ModelAPI$createTopic = F3(
	function (text, iconName, model) {
		var id = model.nextId;
		var topic = A3($author$project$Model$TopicInfo, id, text, iconName);
		return _Utils_Tuple2(
			$author$project$ModelAPI$nextId(
				_Utils_update(
					model,
					{
						items: A3(
							$elm$core$Dict$insert,
							id,
							$author$project$Model$Topic(topic),
							model.items)
					})),
			id);
	});
var $author$project$ModelAPI$getMapIfExists = F2(
	function (mapId, maps) {
		return A2($elm$core$Dict$get, mapId, maps);
	});
var $author$project$ModelAPI$getMap = F2(
	function (mapId, maps) {
		var _v0 = A2($author$project$ModelAPI$getMapIfExists, mapId, maps);
		if (_v0.$ === 'Just') {
			var map = _v0.a;
			return $elm$core$Maybe$Just(map);
		} else {
			return A3($author$project$ModelAPI$illegalMapId, 'getMap', mapId, $elm$core$Maybe$Nothing);
		}
	});
var $author$project$Config$newTopicPos = A2($author$project$Model$Point, 178, 180);
var $author$project$ModelAPI$select = F3(
	function (id, mapId, model) {
		return _Utils_update(
			model,
			{
				selection: _List_fromArray(
					[
						_Utils_Tuple2(id, mapId)
					])
			});
	});
var $author$project$Config$contentFontSize = 13;
var $author$project$Config$topicBorderWidth = 1;
var $author$project$Config$topicDetailPadding = 8;
var $author$project$Config$topicHeight = 28;
var $author$project$Config$topicLineHeight = 1.5;
var $author$project$Config$topicWidth = 156;
var $author$project$Config$topicDetailSize = A2($author$project$Model$Size, $author$project$Config$topicWidth - $author$project$Config$topicHeight, ($author$project$Config$topicLineHeight * $author$project$Config$contentFontSize) + (2 * ($author$project$Config$topicDetailPadding + $author$project$Config$topicBorderWidth)));
var $author$project$ModelAPI$createTopicAndAddToMap = F4(
	function (text, iconName, mapId, model) {
		var _v0 = A2($author$project$ModelAPI$getMap, mapId, model.maps);
		if (_v0.$ === 'Just') {
			var map = _v0.a;
			var props = $author$project$Model$MapTopic(
				A3(
					$author$project$Model$TopicProps,
					A2($author$project$Model$Point, $author$project$Config$newTopicPos.x + map.rect.x1, $author$project$Config$newTopicPos.y + map.rect.y1),
					$author$project$Config$topicDetailSize,
					$author$project$Model$Monad($author$project$Model$LabelOnly)));
			var _v1 = A3($author$project$ModelAPI$createTopic, text, iconName, model);
			var newModel = _v1.a;
			var topicId = _v1.b;
			return A3(
				$author$project$ModelAPI$select,
				topicId,
				mapId,
				A4($author$project$ModelAPI$addItemToMap, topicId, props, mapId, newModel));
		} else {
			return model;
		}
	});
var $author$project$Model$ItemEdit = F2(
	function (a, b) {
		return {$: 'ItemEdit', a: a, b: b};
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Config$topicDetailMaxWidth = 300;
var $author$project$Config$topicH2 = $author$project$Config$topicHeight / 2;
var $author$project$Config$topicSize = A2($author$project$Model$Size, $author$project$Config$topicWidth, $author$project$Config$topicHeight);
var $author$project$Config$topicW2 = $author$project$Config$topicWidth / 2;
var $author$project$MapAutoSize$detailTopicExtent = F6(
	function (topicId, mapId, pos, size, rectAcc, model) {
		var textWidth = _Utils_eq(
			model.editState,
			A2($author$project$Model$ItemEdit, topicId, mapId)) ? $author$project$Config$topicDetailMaxWidth : size.w;
		return A4(
			$author$project$Model$Rectangle,
			A2($elm$core$Basics$min, rectAcc.x1, pos.x - $author$project$Config$topicW2),
			A2($elm$core$Basics$min, rectAcc.y1, pos.y - $author$project$Config$topicH2),
			A2($elm$core$Basics$max, rectAcc.x2, (((pos.x - $author$project$Config$topicW2) + textWidth) + $author$project$Config$topicSize.h) + (2 * $author$project$Config$topicBorderWidth)),
			A2($elm$core$Basics$max, rectAcc.y2, ((pos.y - $author$project$Config$topicH2) + size.h) + (2 * $author$project$Config$topicBorderWidth)));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$ModelAPI$isVisible = function (item) {
	return !item.hidden;
};
var $author$project$MapAutoSize$mapExtent = F3(
	function (pos, rect, rectAcc) {
		var mapWidth = rect.x2 - rect.x1;
		var mapHeight = rect.y2 - rect.y1;
		return A4(
			$author$project$Model$Rectangle,
			A2($elm$core$Basics$min, rectAcc.x1, pos.x - $author$project$Config$topicW2),
			A2($elm$core$Basics$min, rectAcc.y1, pos.y - $author$project$Config$topicH2),
			A2($elm$core$Basics$max, rectAcc.x2, (pos.x - $author$project$Config$topicW2) + mapWidth),
			A2($elm$core$Basics$max, rectAcc.y2, (pos.y + $author$project$Config$topicH2) + mapHeight));
	});
var $author$project$ModelAPI$illegalItemId = F3(
	function (funcName, id, val) {
		return A4($author$project$ModelAPI$illegalId, funcName, 'Item', id, val);
	});
var $author$project$ModelAPI$topicMismatch = F3(
	function (funcName, id, val) {
		return A3(
			$author$project$Utils$logError,
			funcName,
			$elm$core$String$fromInt(id) + ' is not a Topic but an Assoc',
			val);
	});
var $author$project$ModelAPI$updateTopicProps = F4(
	function (topicId, mapId, propsFunc, model) {
		return _Utils_update(
			model,
			{
				maps: A3(
					$author$project$ModelAPI$updateMaps,
					mapId,
					function (map) {
						return _Utils_update(
							map,
							{
								items: A3(
									$elm$core$Dict$update,
									topicId,
									function (mapItem_) {
										if (mapItem_.$ === 'Just') {
											var mapItem = mapItem_.a;
											var _v1 = mapItem.props;
											if (_v1.$ === 'MapTopic') {
												var props = _v1.a;
												return $elm$core$Maybe$Just(
													_Utils_update(
														mapItem,
														{
															props: $author$project$Model$MapTopic(
																propsFunc(props))
														}));
											} else {
												return A3($author$project$ModelAPI$topicMismatch, 'updateTopicProps', topicId, $elm$core$Maybe$Nothing);
											}
										} else {
											return A3($author$project$ModelAPI$illegalItemId, 'updateTopicProps', topicId, $elm$core$Maybe$Nothing);
										}
									},
									map.items)
							});
					},
					model.maps)
			});
	});
var $author$project$ModelAPI$setTopicPosByDelta = F4(
	function (topicId, mapId, delta, model) {
		return A4(
			$author$project$ModelAPI$updateTopicProps,
			topicId,
			mapId,
			function (props) {
				return _Utils_update(
					props,
					{
						pos: A2($author$project$Model$Point, props.pos.x + delta.x, props.pos.y + delta.y)
					});
			},
			model);
	});
var $author$project$MapAutoSize$storeMapRect = F5(
	function (mapId, newRect, oldRect, parentMapId, model) {
		return _Utils_eq(
			mapId,
			$author$project$ModelAPI$activeMap(model)) ? _Utils_Tuple2(newRect, model) : _Utils_Tuple2(
			newRect,
			A4(
				$author$project$ModelAPI$setTopicPosByDelta,
				mapId,
				parentMapId,
				A2($author$project$Model$Point, newRect.x1 - oldRect.x1, newRect.y1 - oldRect.y1),
				_Utils_update(
					model,
					{
						maps: A3(
							$author$project$ModelAPI$updateMaps,
							mapId,
							function (map) {
								return _Utils_update(
									map,
									{rect: newRect});
							},
							model.maps)
					})));
	});
var $author$project$MapAutoSize$topicExtent = F2(
	function (pos, rectAcc) {
		return A4(
			$author$project$Model$Rectangle,
			A2($elm$core$Basics$min, rectAcc.x1, pos.x - $author$project$Config$topicW2),
			A2($elm$core$Basics$min, rectAcc.y1, pos.y - $author$project$Config$topicH2),
			A2($elm$core$Basics$max, rectAcc.x2, (pos.x + $author$project$Config$topicW2) + (2 * $author$project$Config$topicBorderWidth)),
			A2($elm$core$Basics$max, rectAcc.y2, (pos.y + $author$project$Config$topicH2) + (2 * $author$project$Config$topicBorderWidth)));
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Config$whiteBoxPadding = 12;
var $author$project$MapAutoSize$calcItemSize = F4(
	function (mapItem, mapId, rectAcc, model) {
		var _v3 = mapItem.props;
		if (_v3.$ === 'MapTopic') {
			var pos = _v3.a.pos;
			var size = _v3.a.size;
			var displayMode = _v3.a.displayMode;
			if (displayMode.$ === 'Monad') {
				if (displayMode.a.$ === 'LabelOnly') {
					var _v5 = displayMode.a;
					return _Utils_Tuple2(
						A2($author$project$MapAutoSize$topicExtent, pos, rectAcc),
						model);
				} else {
					var _v6 = displayMode.a;
					return _Utils_Tuple2(
						A6($author$project$MapAutoSize$detailTopicExtent, mapItem.id, mapId, pos, size, rectAcc, model),
						model);
				}
			} else {
				switch (displayMode.a.$) {
					case 'BlackBox':
						var _v7 = displayMode.a;
						return _Utils_Tuple2(
							A2($author$project$MapAutoSize$topicExtent, pos, rectAcc),
							model);
					case 'WhiteBox':
						var _v8 = displayMode.a;
						var _v9 = A2($author$project$MapAutoSize$calcMapRect, mapItem.id, model);
						var rect = _v9.a;
						var model_ = _v9.b;
						return _Utils_Tuple2(
							A3($author$project$MapAutoSize$mapExtent, pos, rect, rectAcc),
							model_);
					default:
						var _v10 = displayMode.a;
						return _Utils_Tuple2(
							A2($author$project$MapAutoSize$topicExtent, pos, rectAcc),
							model);
				}
			}
		} else {
			return _Utils_Tuple2(rectAcc, model);
		}
	});
var $author$project$MapAutoSize$calcMapRect = F2(
	function (mapId, model) {
		var _v0 = A2($author$project$ModelAPI$getMap, mapId, model.maps);
		if (_v0.$ === 'Just') {
			var map = _v0.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				F2(
					function (mapItem, _v2) {
						var rectAcc = _v2.a;
						var modelAcc = _v2.b;
						return A4($author$project$MapAutoSize$calcItemSize, mapItem, mapId, rectAcc, modelAcc);
					}),
				_Utils_Tuple2(
					A4($author$project$Model$Rectangle, 5000, 5000, -5000, -5000),
					model),
				A2(
					$elm$core$List$filter,
					$author$project$ModelAPI$isVisible,
					$elm$core$Dict$values(map.items)));
			var rect = _v1.a;
			var model_ = _v1.b;
			var newRect = A4($author$project$Model$Rectangle, rect.x1 - $author$project$Config$whiteBoxPadding, rect.y1 - $author$project$Config$whiteBoxPadding, rect.x2 + $author$project$Config$whiteBoxPadding, rect.y2 + $author$project$Config$whiteBoxPadding);
			return A5($author$project$MapAutoSize$storeMapRect, mapId, newRect, map.rect, map.parentMapId, model_);
		} else {
			return _Utils_Tuple2(
				A4($author$project$Model$Rectangle, 0, 0, 0, 0),
				model);
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$MapAutoSize$autoSize = function (model) {
	return A2(
		$author$project$MapAutoSize$calcMapRect,
		$author$project$ModelAPI$activeMap(model),
		model).b;
};
var $author$project$ModelAPI$getItemId = function (item) {
	if (item.$ === 'Topic') {
		var id = item.a.id;
		return id;
	} else {
		var id = item.a.id;
		return id;
	}
};
var $author$project$ModelAPI$assocMismatch = F3(
	function (funcName, id, val) {
		return A3(
			$author$project$Utils$logError,
			funcName,
			$elm$core$String$fromInt(id) + ' is not an Assoc but a Topic',
			val);
	});
var $author$project$ModelAPI$getAssocInfo = F2(
	function (assocId, model) {
		var _v0 = A2($elm$core$Dict$get, assocId, model.items);
		if (_v0.$ === 'Just') {
			var item = _v0.a;
			if (item.$ === 'Topic') {
				return A3($author$project$ModelAPI$assocMismatch, 'getAssocInfo', assocId, $elm$core$Maybe$Nothing);
			} else {
				var assoc = item.a;
				return $elm$core$Maybe$Just(assoc);
			}
		} else {
			return A3($author$project$ModelAPI$illegalItemId, 'getAssocInfo', assocId, $elm$core$Maybe$Nothing);
		}
	});
var $author$project$ModelAPI$hasPlayer = F3(
	function (playerId, model, assocId) {
		var _v0 = A2($author$project$ModelAPI$getAssocInfo, assocId, model);
		if (_v0.$ === 'Just') {
			var assoc = _v0.a;
			return _Utils_eq(assoc.player1, playerId) || _Utils_eq(assoc.player2, playerId);
		} else {
			return false;
		}
	});
var $author$project$ModelAPI$isTopic = function (item) {
	if (item.$ === 'Topic') {
		return true;
	} else {
		return false;
	}
};
var $author$project$ModelAPI$isAssoc = function (item) {
	return !$author$project$ModelAPI$isTopic(item);
};
var $author$project$ModelAPI$assocsOfPlayer = F2(
	function (playerId, model) {
		return A2(
			$elm$core$List$filter,
			A2($author$project$ModelAPI$hasPlayer, playerId, model),
			A2(
				$elm$core$List$map,
				$author$project$ModelAPI$getItemId,
				A2(
					$elm$core$List$filter,
					$author$project$ModelAPI$isAssoc,
					$elm$core$Dict$values(model.items))));
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$ModelAPI$deleteItem = F2(
	function (itemId, model) {
		return A3(
			$elm$core$List$foldr,
			$author$project$ModelAPI$deleteItem,
			_Utils_update(
				model,
				{
					items: A2($elm$core$Dict$remove, itemId, model.items),
					maps: A2(
						$elm$core$Dict$map,
						F2(
							function (_v0, map) {
								return _Utils_update(
									map,
									{
										items: A2($elm$core$Dict$remove, itemId, map.items)
									});
							}),
						model.maps)
				}),
			A2($author$project$ModelAPI$assocsOfPlayer, itemId, model));
	});
var $author$project$Main$delete = function (model) {
	var newModel = A3(
		$elm$core$List$foldr,
		F2(
			function (itemId, modelAcc) {
				return A2($author$project$ModelAPI$deleteItem, itemId, modelAcc);
			}),
		model,
		A2($elm$core$List$map, $elm$core$Tuple$first, model.selection));
	return $author$project$MapAutoSize$autoSize(
		_Utils_update(
			newModel,
			{selection: _List_Nil}));
};
var $author$project$ModelAPI$isMapTopic = function (item) {
	var _v0 = item.props;
	if (_v0.$ === 'MapTopic') {
		return true;
	} else {
		return false;
	}
};
var $author$project$ModelAPI$isMapAssoc = function (item) {
	return !$author$project$ModelAPI$isMapTopic(item);
};
var $author$project$ModelAPI$mapAssocsOfPlayer_ = F3(
	function (playerId, items, model) {
		return A2(
			$elm$core$List$filter,
			A2($author$project$ModelAPI$hasPlayer, playerId, model),
			A2(
				$elm$core$List$map,
				function ($) {
					return $.id;
				},
				A2(
					$elm$core$List$filter,
					$author$project$ModelAPI$isMapAssoc,
					$elm$core$Dict$values(items))));
	});
var $author$project$ModelAPI$hideItem_ = F3(
	function (itemId, items, model) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (assocId, itemsAcc) {
					return A3($author$project$ModelAPI$hideItem_, assocId, itemsAcc, model);
				}),
			A3(
				$elm$core$Dict$update,
				itemId,
				function (item_) {
					if (item_.$ === 'Just') {
						var item = item_.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								item,
								{hidden: true}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				},
				items),
			A3($author$project$ModelAPI$mapAssocsOfPlayer_, itemId, items, model));
	});
var $author$project$ModelAPI$hideItem = F3(
	function (itemId, mapId, model) {
		return _Utils_update(
			model,
			{
				maps: A3(
					$author$project$ModelAPI$updateMaps,
					mapId,
					function (map) {
						return _Utils_update(
							map,
							{
								items: A3($author$project$ModelAPI$hideItem_, itemId, map.items, model)
							});
					},
					model.maps)
			});
	});
var $author$project$Main$hide = function (model) {
	var newModel = A3(
		$elm$core$List$foldr,
		F2(
			function (_v0, modelAcc) {
				var itemId = _v0.a;
				var mapId = _v0.b;
				return A3($author$project$ModelAPI$hideItem, itemId, mapId, modelAcc);
			}),
		model,
		model.selection);
	return $author$project$MapAutoSize$autoSize(
		_Utils_update(
			newModel,
			{selection: _List_Nil}));
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $author$project$ModelAPI$hasMap = F2(
	function (mapId, maps) {
		return A2($elm$core$Dict$member, mapId, maps);
	});
var $author$project$ModelAPI$setDisplayMode = F4(
	function (topicId, mapId, displayMode, model) {
		return A4(
			$author$project$ModelAPI$updateTopicProps,
			topicId,
			mapId,
			function (props) {
				return _Utils_update(
					props,
					{displayMode: displayMode});
			},
			model);
	});
var $author$project$Main$createMapIfNeeded = F3(
	function (topicId, mapId, model) {
		return A2($author$project$ModelAPI$hasMap, topicId, model.maps) ? _Utils_Tuple2(model, false) : _Utils_Tuple2(
			A4(
				$author$project$ModelAPI$setDisplayMode,
				topicId,
				mapId,
				$author$project$Model$Container($author$project$Model$BlackBox),
				_Utils_update(
					model,
					{
						maps: A3(
							$elm$core$Dict$insert,
							topicId,
							A4(
								$author$project$Model$Map,
								topicId,
								$elm$core$Dict$empty,
								A4($author$project$Model$Rectangle, 0, 0, 0, 0),
								mapId),
							model.maps)
					})),
			true);
	});
var $author$project$Utils$fail = F3(
	function (funcName, args, val) {
		return A2(
			$elm$core$Debug$log,
			'--> @' + (funcName + (' failed ' + $author$project$Utils$toString(args))),
			val);
	});
var $author$project$ModelAPI$itemNotInMap = F4(
	function (funcName, itemId, mapId, val) {
		return A3(
			$author$project$Utils$logError,
			funcName,
			'item ' + ($elm$core$String$fromInt(itemId) + (' not in map ' + $elm$core$String$fromInt(mapId))),
			val);
	});
var $author$project$ModelAPI$getMapItem = F2(
	function (itemId, map) {
		var _v0 = A2($elm$core$Dict$get, itemId, map.items);
		if (_v0.$ === 'Just') {
			var mapItem = _v0.a;
			return $elm$core$Maybe$Just(mapItem);
		} else {
			return A4($author$project$ModelAPI$itemNotInMap, 'getMapItem', itemId, map.id, $elm$core$Maybe$Nothing);
		}
	});
var $author$project$ModelAPI$getMapItemById = F3(
	function (itemId, mapId, maps) {
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$ModelAPI$getMapItem(itemId),
			A2($author$project$ModelAPI$getMap, mapId, maps));
	});
var $author$project$ModelAPI$getTopicProps = F3(
	function (topicId, mapId, maps) {
		var _v0 = A3($author$project$ModelAPI$getMapItemById, topicId, mapId, maps);
		if (_v0.$ === 'Just') {
			var mapItem = _v0.a;
			var _v1 = mapItem.props;
			if (_v1.$ === 'MapTopic') {
				var props = _v1.a;
				return $elm$core$Maybe$Just(props);
			} else {
				return A3($author$project$ModelAPI$topicMismatch, 'getTopicProps', topicId, $elm$core$Maybe$Nothing);
			}
		} else {
			return A3(
				$author$project$Utils$fail,
				'getTopicProps',
				{mapId: mapId, topicId: topicId},
				$elm$core$Maybe$Nothing);
		}
	});
var $author$project$ModelAPI$setTopicPos = F4(
	function (topicId, mapId, pos, model) {
		return A4(
			$author$project$ModelAPI$updateTopicProps,
			topicId,
			mapId,
			function (props) {
				return _Utils_update(
					props,
					{pos: pos});
			},
			model);
	});
var $author$project$Main$moveTopicToMap = F7(
	function (topicId, mapId, origPos, targetId, targetMapId, pos, model) {
		var _v0 = A3($author$project$Main$createMapIfNeeded, targetId, targetMapId, model);
		var newModel = _v0.a;
		var created = _v0.b;
		var newPos = function () {
			if (created) {
				return A2($author$project$Model$Point, $author$project$Config$topicW2 + $author$project$Config$whiteBoxPadding, $author$project$Config$topicH2 + $author$project$Config$whiteBoxPadding);
			} else {
				return pos;
			}
		}();
		var props_ = A2(
			$elm$core$Maybe$andThen,
			function (props) {
				return $elm$core$Maybe$Just(
					$author$project$Model$MapTopic(
						_Utils_update(
							props,
							{pos: newPos})));
			},
			A3($author$project$ModelAPI$getTopicProps, topicId, mapId, newModel.maps));
		if (props_.$ === 'Just') {
			var props = props_.a;
			return $author$project$MapAutoSize$autoSize(
				A3(
					$author$project$ModelAPI$select,
					targetId,
					targetMapId,
					A4(
						$author$project$ModelAPI$addItemToMap,
						topicId,
						props,
						targetId,
						A4(
							$author$project$ModelAPI$setTopicPos,
							topicId,
							mapId,
							origPos,
							A3($author$project$ModelAPI$hideItem, topicId, mapId, newModel)))));
		} else {
			return model;
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Storage$encodeItem = function (item) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				function () {
				if (item.$ === 'Topic') {
					var topic = item.a;
					return _Utils_Tuple2(
						'topic',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'id',
									$elm$json$Json$Encode$int(topic.id)),
									_Utils_Tuple2(
									'text',
									$elm$json$Json$Encode$string(topic.text)),
									_Utils_Tuple2(
									'iconName',
									$elm$json$Json$Encode$string(
										A2($elm$core$Maybe$withDefault, '', topic.iconName)))
								])));
				} else {
					var assoc = item.a;
					return _Utils_Tuple2(
						'assoc',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'id',
									$elm$json$Json$Encode$int(assoc.id)),
									_Utils_Tuple2(
									'itemType',
									$elm$json$Json$Encode$string(assoc.itemType)),
									_Utils_Tuple2(
									'player1',
									$elm$json$Json$Encode$int(assoc.player1)),
									_Utils_Tuple2(
									'role1',
									$elm$json$Json$Encode$string(assoc.role1)),
									_Utils_Tuple2(
									'player2',
									$elm$json$Json$Encode$int(assoc.player2)),
									_Utils_Tuple2(
									'role2',
									$elm$json$Json$Encode$string(assoc.role2))
								])));
				}
			}()
			]));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$Storage$encodeDisplayName = function (displayMode) {
	return $elm$json$Json$Encode$string(
		function () {
			if (displayMode.$ === 'Monad') {
				if (displayMode.a.$ === 'LabelOnly') {
					var _v1 = displayMode.a;
					return 'LabelOnly';
				} else {
					var _v2 = displayMode.a;
					return 'Detail';
				}
			} else {
				switch (displayMode.a.$) {
					case 'BlackBox':
						var _v3 = displayMode.a;
						return 'BlackBox';
					case 'WhiteBox':
						var _v4 = displayMode.a;
						return 'WhiteBox';
					default:
						var _v5 = displayMode.a;
						return 'Unboxed';
				}
			}
		}());
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Storage$encodeMapItem = function (item) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(item.id)),
				_Utils_Tuple2(
				'hidden',
				$elm$json$Json$Encode$bool(item.hidden)),
				_Utils_Tuple2(
				'pinned',
				$elm$json$Json$Encode$bool(item.pinned)),
				function () {
				var _v0 = item.props;
				if (_v0.$ === 'MapTopic') {
					var topicProps = _v0.a;
					return _Utils_Tuple2(
						'topicProps',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'pos',
									$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'x',
												$elm$json$Json$Encode$float(topicProps.pos.x)),
												_Utils_Tuple2(
												'y',
												$elm$json$Json$Encode$float(topicProps.pos.y))
											]))),
									_Utils_Tuple2(
									'size',
									$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'w',
												$elm$json$Json$Encode$float(topicProps.size.w)),
												_Utils_Tuple2(
												'h',
												$elm$json$Json$Encode$float(topicProps.size.h))
											]))),
									_Utils_Tuple2(
									'displayMode',
									$author$project$Storage$encodeDisplayName(topicProps.displayMode))
								])));
				} else {
					var assosProps = _v0.a;
					return _Utils_Tuple2(
						'assocProps',
						$elm$json$Json$Encode$object(_List_Nil));
				}
			}(),
				_Utils_Tuple2(
				'parentAssocId',
				$elm$json$Json$Encode$int(item.parentAssocId))
			]));
};
var $author$project$Storage$encodeMap = function (map) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(map.id)),
				_Utils_Tuple2(
				'items',
				A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $author$project$Storage$encodeMapItem, map.items)),
				_Utils_Tuple2(
				'rect',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'x1',
							$elm$json$Json$Encode$float(map.rect.x1)),
							_Utils_Tuple2(
							'y1',
							$elm$json$Json$Encode$float(map.rect.y1)),
							_Utils_Tuple2(
							'x2',
							$elm$json$Json$Encode$float(map.rect.x2)),
							_Utils_Tuple2(
							'y2',
							$elm$json$Json$Encode$float(map.rect.y2))
						]))),
				_Utils_Tuple2(
				'parentMapId',
				$elm$json$Json$Encode$int(map.parentMapId))
			]));
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Storage$encodeModel = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'items',
				A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $author$project$Storage$encodeItem, model.items)),
				_Utils_Tuple2(
				'maps',
				A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $author$project$Storage$encodeMap, model.maps)),
				_Utils_Tuple2(
				'mapPath',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$int, model.mapPath)),
				_Utils_Tuple2(
				'nextId',
				$elm$json$Json$Encode$int(model.nextId))
			]));
};
var $author$project$Storage$store = _Platform_outgoingPort('store', $elm$core$Basics$identity);
var $author$project$Storage$storeModel = function (model) {
	return _Utils_Tuple2(
		model,
		$author$project$Storage$store(
			$author$project$Storage$encodeModel(model)));
};
var $author$project$Boxing$boxItems = F3(
	function (containerItems, targetItems, model) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (containerItem, targetItemsAcc) {
					var _v0 = A2($elm$core$Dict$get, containerItem.id, targetItemsAcc);
					if (_v0.$ === 'Just') {
						var pinned = _v0.a.pinned;
						if (pinned) {
							return A3($author$project$ModelAPI$hideItem_, containerItem.parentAssocId, targetItemsAcc, model);
						} else {
							var items = A3($author$project$ModelAPI$hideItem_, containerItem.id, targetItemsAcc, model);
							var _v1 = A2($author$project$ModelAPI$getMapIfExists, containerItem.id, model.maps);
							if (_v1.$ === 'Just') {
								var map = _v1.a;
								return A3($author$project$Boxing$boxItems, map.items, items, model);
							} else {
								return items;
							}
						}
					} else {
						return targetItemsAcc;
					}
				}),
			targetItems,
			$elm$core$Dict$values(containerItems));
	});
var $author$project$ModelAPI$getDisplayMode = F3(
	function (topicId, mapId, maps) {
		var _v0 = A3($author$project$ModelAPI$getTopicProps, topicId, mapId, maps);
		if (_v0.$ === 'Just') {
			var displayMode = _v0.a.displayMode;
			return $elm$core$Maybe$Just(displayMode);
		} else {
			return A3(
				$author$project$Utils$fail,
				'getDisplayMode',
				{mapId: mapId, topicId: topicId},
				$elm$core$Maybe$Nothing);
		}
	});
var $author$project$Boxing$transferContent = F4(
	function (containerId, targetMapId, transferFunc, model) {
		var _v0 = A2($author$project$ModelAPI$getMap, containerId, model.maps);
		if (_v0.$ === 'Just') {
			var containerMap = _v0.a;
			return A3(
				$author$project$ModelAPI$updateMaps,
				targetMapId,
				function (targetMap) {
					return _Utils_update(
						targetMap,
						{
							items: A3(transferFunc, containerMap.items, targetMap.items, model)
						});
				},
				model.maps);
		} else {
			return model.maps;
		}
	});
var $author$project$Boxing$boxContainer = F3(
	function (containerId, targetMapId, model) {
		var _v0 = A3($author$project$ModelAPI$getDisplayMode, containerId, targetMapId, model.maps);
		if (((_v0.$ === 'Just') && (_v0.a.$ === 'Container')) && (_v0.a.a.$ === 'Unboxed')) {
			var _v1 = _v0.a.a;
			return A4($author$project$Boxing$transferContent, containerId, targetMapId, $author$project$Boxing$boxItems, model);
		} else {
			return model.maps;
		}
	});
var $author$project$ModelAPI$getSingleSelection = function (model) {
	var _v0 = model.selection;
	if (_v0.b && (!_v0.b.b)) {
		var selItem = _v0.a;
		return $elm$core$Maybe$Just(selItem);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Boxing$targetAssocItem = F2(
	function (assocId, targetItems) {
		var _v0 = A2($elm$core$Dict$get, assocId, targetItems);
		if (_v0.$ === 'Just') {
			var item = _v0.a;
			return _Utils_update(
				item,
				{hidden: false});
		} else {
			return A5(
				$author$project$Model$MapItem,
				assocId,
				false,
				false,
				$author$project$Model$MapAssoc($author$project$Model$AssocProps),
				-1);
		}
	});
var $author$project$Boxing$unboxAssoc = F2(
	function (containerItem, targetItems) {
		var assocToInsert = A2($author$project$Boxing$targetAssocItem, containerItem.id, targetItems);
		return A3($elm$core$Dict$insert, assocToInsert.id, assocToInsert, targetItems);
	});
var $author$project$Boxing$isAbort = function (item) {
	var _v0 = item.props;
	if (_v0.$ === 'MapTopic') {
		var props = _v0.a;
		var _v1 = props.displayMode;
		if (_v1.$ === 'Container') {
			switch (_v1.a.$) {
				case 'BlackBox':
					var _v2 = _v1.a;
					return true;
				case 'WhiteBox':
					var _v3 = _v1.a;
					return true;
				default:
					var _v4 = _v1.a;
					return false;
			}
		} else {
			return false;
		}
	} else {
		return false;
	}
};
var $author$project$Boxing$setUnboxed = function (item) {
	return _Utils_update(
		item,
		{
			props: function () {
				var _v0 = item.props;
				if (_v0.$ === 'MapTopic') {
					var props = _v0.a;
					return $author$project$Model$MapTopic(
						_Utils_update(
							props,
							{
								displayMode: $author$project$Model$Container($author$project$Model$Unboxed)
							}));
				} else {
					var props = _v0.a;
					return $author$project$Model$MapAssoc(props);
				}
			}()
		});
};
var $author$project$Boxing$unboxTopic = F3(
	function (containerItem, targetItems, model) {
		var assocToInsert = A2($author$project$Boxing$targetAssocItem, containerItem.parentAssocId, targetItems);
		var _v0 = function () {
			var _v1 = A2($elm$core$Dict$get, containerItem.id, targetItems);
			if (_v1.$ === 'Just') {
				var item = _v1.a;
				var _v2 = A2(
					$author$project$Utils$info,
					'unboxTopic',
					_Utils_update(
						item,
						{hidden: false, pinned: !item.hidden}));
				return _Utils_Tuple2(
					_Utils_update(
						item,
						{hidden: false, pinned: !item.hidden}),
					$author$project$Boxing$isAbort(item));
			} else {
				return A2($author$project$ModelAPI$hasMap, containerItem.id, model.maps) ? _Utils_Tuple2(
					$author$project$Boxing$setUnboxed(containerItem),
					false) : _Utils_Tuple2(containerItem, false);
			}
		}();
		var topicToInsert = _v0.a;
		var abort = _v0.b;
		return _Utils_Tuple2(
			A3(
				$elm$core$Dict$insert,
				assocToInsert.id,
				assocToInsert,
				A3($elm$core$Dict$insert, topicToInsert.id, topicToInsert, targetItems)),
			abort);
	});
var $author$project$Boxing$unboxItems = F3(
	function (containerItems, targetItems, model) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (containerItem, targetItemsAcc) {
					var _v0 = containerItem.props;
					if (_v0.$ === 'MapTopic') {
						var _v1 = A3($author$project$Boxing$unboxTopic, containerItem, targetItemsAcc, model);
						var items = _v1.a;
						var abort = _v1.b;
						if (abort) {
							return items;
						} else {
							var _v2 = A2($author$project$ModelAPI$getMapIfExists, containerItem.id, model.maps);
							if (_v2.$ === 'Just') {
								var map = _v2.a;
								return A3($author$project$Boxing$unboxItems, map.items, items, model);
							} else {
								return items;
							}
						}
					} else {
						return A2($author$project$Boxing$unboxAssoc, containerItem, targetItemsAcc);
					}
				}),
			targetItems,
			A2(
				$elm$core$List$filter,
				$author$project$ModelAPI$isVisible,
				$elm$core$Dict$values(containerItems)));
	});
var $author$project$Boxing$unboxContainer = F3(
	function (containerId, targetMapId, model) {
		var _v0 = A3($author$project$ModelAPI$getDisplayMode, containerId, targetMapId, model.maps);
		_v0$2:
		while (true) {
			if ((_v0.$ === 'Just') && (_v0.a.$ === 'Container')) {
				switch (_v0.a.a.$) {
					case 'BlackBox':
						var _v1 = _v0.a.a;
						return A4($author$project$Boxing$transferContent, containerId, targetMapId, $author$project$Boxing$unboxItems, model);
					case 'WhiteBox':
						var _v2 = _v0.a.a;
						return A4($author$project$Boxing$transferContent, containerId, targetMapId, $author$project$Boxing$unboxItems, model);
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return model.maps;
	});
var $author$project$Main$switchDisplay = F2(
	function (displayMode, model) {
		return $author$project$MapAutoSize$autoSize(
			function () {
				var _v0 = $author$project$ModelAPI$getSingleSelection(model);
				if (_v0.$ === 'Just') {
					var _v1 = _v0.a;
					var containerId = _v1.a;
					var targetMapId = _v1.b;
					return A4(
						$author$project$ModelAPI$setDisplayMode,
						containerId,
						targetMapId,
						displayMode,
						_Utils_update(
							model,
							{
								maps: function () {
									if (displayMode.$ === 'Monad') {
										return model.maps;
									} else {
										switch (displayMode.a.$) {
											case 'BlackBox':
												var _v3 = displayMode.a;
												return A3($author$project$Boxing$boxContainer, containerId, targetMapId, model);
											case 'WhiteBox':
												var _v4 = displayMode.a;
												return A3($author$project$Boxing$boxContainer, containerId, targetMapId, model);
											default:
												var _v5 = displayMode.a;
												return A3($author$project$Boxing$unboxContainer, containerId, targetMapId, model);
										}
									}
								}()
							}));
				} else {
					return model;
				}
			}());
	});
var $author$project$Config$topicDefaultText = 'New Topic';
var $author$project$Main$endEdit = function (model) {
	return $author$project$MapAutoSize$autoSize(
		_Utils_update(
			model,
			{editState: $author$project$Model$NoEdit}));
};
var $author$project$ModelAPI$updateTopicInfo = F3(
	function (topicId, topicFunc, model) {
		return _Utils_update(
			model,
			{
				items: A3(
					$elm$core$Dict$update,
					topicId,
					function (maybeItem) {
						if (maybeItem.$ === 'Just') {
							var item = maybeItem.a;
							if (item.$ === 'Topic') {
								var topic = item.a;
								return $elm$core$Maybe$Just(
									$author$project$Model$Topic(
										topicFunc(topic)));
							} else {
								return A3($author$project$ModelAPI$topicMismatch, 'updateTopicInfo', topicId, $elm$core$Maybe$Nothing);
							}
						} else {
							return A3($author$project$ModelAPI$illegalItemId, 'updateTopicInfo', topicId, $elm$core$Maybe$Nothing);
						}
					},
					model.items)
			});
	});
var $author$project$Main$onTextInput = F2(
	function (text, model) {
		var _v0 = model.editState;
		if (_v0.$ === 'ItemEdit') {
			var topicId = _v0.a;
			return A3(
				$author$project$ModelAPI$updateTopicInfo,
				topicId,
				function (topic) {
					return _Utils_update(
						topic,
						{text: text});
				},
				model);
		} else {
			return A3($author$project$Utils$logError, 'onTextInput', 'called when editState is NoEdit', model);
		}
	});
var $author$project$AppModel$Edit = function (a) {
	return {$: 'Edit', a: a};
};
var $author$project$AppModel$NoOp = {$: 'NoOp'};
var $author$project$Model$SetTopicSize = F3(
	function (a, b, c) {
		return {$: 'SetTopicSize', a: a, b: b, c: c};
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $elm$browser$Browser$Dom$getElement = _Browser_getElement;
var $author$project$Main$measureText = F4(
	function (text, topicId, mapId, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{measureText: text}),
			A2(
				$elm$core$Task$attempt,
				function (result) {
					if (result.$ === 'Ok') {
						var elem = result.a;
						return $author$project$AppModel$Edit(
							A3(
								$author$project$Model$SetTopicSize,
								topicId,
								mapId,
								A2($author$project$Model$Size, elem.element.width, elem.element.height)));
					} else {
						var err = result.a;
						return A3(
							$author$project$Utils$logError,
							'measureText',
							$author$project$Utils$toString(err),
							$author$project$AppModel$NoOp);
					}
				},
				$elm$browser$Browser$Dom$getElement('measure')));
	});
var $author$project$Main$onTextareaInput = F2(
	function (text, model) {
		var _v0 = model.editState;
		if (_v0.$ === 'ItemEdit') {
			var topicId = _v0.a;
			var mapId = _v0.b;
			return A4(
				$author$project$Main$measureText,
				text,
				topicId,
				mapId,
				A3(
					$author$project$ModelAPI$updateTopicInfo,
					topicId,
					function (topic) {
						return _Utils_update(
							topic,
							{text: text});
					},
					model));
		} else {
			return A3(
				$author$project$Utils$logError,
				'onTextareaInput',
				'called when editState is NoEdit',
				_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
		}
	});
var $author$project$ModelAPI$setTopicSize = F4(
	function (topicId, mapId, size, model) {
		return A4(
			$author$project$ModelAPI$updateTopicProps,
			topicId,
			mapId,
			function (props) {
				return _Utils_update(
					props,
					{size: size});
			},
			model);
	});
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $author$project$Main$focus = function (model) {
	var nodeId = function () {
		var _v1 = model.editState;
		if (_v1.$ === 'ItemEdit') {
			var id = _v1.a;
			var mapId = _v1.b;
			return 'dmx-input-' + ($elm$core$String$fromInt(id) + ('-' + $elm$core$String$fromInt(mapId)));
		} else {
			return A3($author$project$Utils$logError, 'focus', 'called when editState is NoEdit', '');
		}
	}();
	return A2(
		$elm$core$Task$attempt,
		function (result) {
			if (result.$ === 'Ok') {
				return $author$project$AppModel$NoOp;
			} else {
				var e = result.a;
				return A3(
					$author$project$Utils$logError,
					'focus',
					$author$project$Utils$toString(e),
					$author$project$AppModel$NoOp);
			}
		},
		$elm$browser$Browser$Dom$focus(nodeId));
};
var $author$project$Main$setDetailDisplayIfMonade = F3(
	function (topicId, mapId, model) {
		return A4(
			$author$project$ModelAPI$updateTopicProps,
			topicId,
			mapId,
			function (props) {
				var _v0 = props.displayMode;
				if (_v0.$ === 'Monad') {
					return _Utils_update(
						props,
						{
							displayMode: $author$project$Model$Monad($author$project$Model$Detail)
						});
				} else {
					return props;
				}
			},
			model);
	});
var $author$project$Main$startEdit = function (model) {
	var newModel = function () {
		var _v0 = $author$project$ModelAPI$getSingleSelection(model);
		if (_v0.$ === 'Just') {
			var _v1 = _v0.a;
			var topicId = _v1.a;
			var mapId = _v1.b;
			return $author$project$MapAutoSize$autoSize(
				A3(
					$author$project$Main$setDetailDisplayIfMonade,
					topicId,
					mapId,
					_Utils_update(
						model,
						{
							editState: A2($author$project$Model$ItemEdit, topicId, mapId)
						})));
		} else {
			return model;
		}
	}();
	return _Utils_Tuple2(
		newModel,
		$author$project$Main$focus(newModel));
};
var $author$project$Storage$storeModelWith = function (_v0) {
	var model = _v0.a;
	var cmd = _v0.b;
	return _Utils_Tuple2(
		model,
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					cmd,
					$author$project$Storage$store(
					$author$project$Storage$encodeModel(model))
				])));
};
var $author$project$Main$updateEdit = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'EditStart':
				return $author$project$Main$startEdit(model);
			case 'OnTextInput':
				var text = msg.a;
				return $author$project$Storage$storeModel(
					A2($author$project$Main$onTextInput, text, model));
			case 'OnTextareaInput':
				var text = msg.a;
				return $author$project$Storage$storeModelWith(
					A2($author$project$Main$onTextareaInput, text, model));
			case 'SetTopicSize':
				var topicId = msg.a;
				var mapId = msg.b;
				var size = msg.c;
				return _Utils_Tuple2(
					$author$project$MapAutoSize$autoSize(
						A4($author$project$ModelAPI$setTopicSize, topicId, mapId, size, model)),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					$author$project$Main$endEdit(model),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$IconMenuAPI$closeIconMenu = function (model) {
	var iconMenu = model.iconMenu;
	return _Utils_update(
		model,
		{
			iconMenu: _Utils_update(
				iconMenu,
				{open: false})
		});
};
var $author$project$IconMenuAPI$openIconMenu = function (model) {
	var iconMenu = model.iconMenu;
	return _Utils_update(
		model,
		{
			iconMenu: _Utils_update(
				iconMenu,
				{open: true})
		});
};
var $author$project$IconMenuAPI$setIcon = F2(
	function (iconName, model) {
		var _v0 = $author$project$ModelAPI$getSingleSelection(model);
		if (_v0.$ === 'Just') {
			var _v1 = _v0.a;
			var id = _v1.a;
			return A3(
				$author$project$ModelAPI$updateTopicInfo,
				id,
				function (topic) {
					return _Utils_update(
						topic,
						{iconName: iconName});
				},
				model);
		} else {
			return model;
		}
	});
var $author$project$IconMenuAPI$updateIconMenu = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Open':
				return _Utils_Tuple2(
					$author$project$IconMenuAPI$openIconMenu(model),
					$elm$core$Platform$Cmd$none);
			case 'Close':
				return _Utils_Tuple2(
					$author$project$IconMenuAPI$closeIconMenu(model),
					$elm$core$Platform$Cmd$none);
			default:
				var maybeIcon = msg.a;
				return $author$project$Storage$storeModel(
					$author$project$IconMenuAPI$closeIconMenu(
						A2($author$project$IconMenuAPI$setIcon, maybeIcon, model)));
		}
	});
var $author$project$SearchAPI$closeResultMenu = function (model) {
	var search = model.search;
	return _Utils_update(
		model,
		{
			search: _Utils_update(
				search,
				{menu: $author$project$Search$Closed})
		});
};
var $author$project$MouseAPI$mouseDown = function (model) {
	return $author$project$SearchAPI$closeResultMenu(
		$author$project$IconMenuAPI$closeIconMenu(
			_Utils_update(
				model,
				{selection: _List_Nil})));
};
var $author$project$Mouse$Time = function (a) {
	return {$: 'Time', a: a};
};
var $author$project$Mouse$WaitForStartTime = F4(
	function (a, b, c, d) {
		return {$: 'WaitForStartTime', a: a, b: b, c: c, d: d};
	});
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$MouseAPI$updateDragState = F2(
	function (model, dragState) {
		var mouse = model.mouse;
		return _Utils_update(
			model,
			{
				mouse: _Utils_update(
					mouse,
					{dragState: dragState})
			});
	});
var $author$project$MouseAPI$mouseDownOnItem = F5(
	function (model, _class, id, mapId, pos) {
		return _Utils_Tuple2(
			A3(
				$author$project$ModelAPI$select,
				id,
				mapId,
				A2(
					$author$project$MouseAPI$updateDragState,
					model,
					A4($author$project$Mouse$WaitForStartTime, _class, id, mapId, pos))),
			A2(
				$elm$core$Task$perform,
				A2($elm$core$Basics$composeL, $author$project$AppModel$Mouse, $author$project$Mouse$Time),
				$elm$time$Time$now));
	});
var $author$project$Mouse$WaitForEndTime = F5(
	function (a, b, c, d, e) {
		return {$: 'WaitForEndTime', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Mouse$Drag = F6(
	function (a, b, c, d, e, f) {
		return {$: 'Drag', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $author$project$MouseAPI$performDrag = F2(
	function (model, pos) {
		var _v0 = model.mouse.dragState;
		if (_v0.$ === 'Drag') {
			var dragMode = _v0.a;
			var id = _v0.b;
			var mapId = _v0.c;
			var origPos = _v0.d;
			var lastPos = _v0.e;
			var target = _v0.f;
			var delta = A2($author$project$Model$Point, pos.x - lastPos.x, pos.y - lastPos.y);
			var newModel = function () {
				if (dragMode.$ === 'DragTopic') {
					return A4($author$project$ModelAPI$setTopicPosByDelta, id, mapId, delta, model);
				} else {
					return model;
				}
			}();
			return $author$project$MapAutoSize$autoSize(
				A2(
					$author$project$MouseAPI$updateDragState,
					newModel,
					A6($author$project$Mouse$Drag, dragMode, id, mapId, origPos, pos, target)));
		} else {
			return A3(
				$author$project$Utils$logError,
				'performDrag',
				'Received \"Move\" message when dragState is ' + $author$project$Utils$toString(model.mouse.dragState),
				model);
		}
	});
var $author$project$MouseAPI$mouseMove = F2(
	function (model, pos) {
		var _v0 = model.mouse.dragState;
		switch (_v0.$) {
			case 'DragEngaged':
				var time = _v0.a;
				var _class = _v0.b;
				var id = _v0.c;
				var mapId = _v0.d;
				var pos_ = _v0.e;
				return _Utils_Tuple2(
					A2(
						$author$project$MouseAPI$updateDragState,
						model,
						A5($author$project$Mouse$WaitForEndTime, time, _class, id, mapId, pos_)),
					A2(
						$elm$core$Task$perform,
						A2($elm$core$Basics$composeL, $author$project$AppModel$Mouse, $author$project$Mouse$Time),
						$elm$time$Time$now));
			case 'WaitForEndTime':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'Drag':
				return _Utils_Tuple2(
					A2($author$project$MouseAPI$performDrag, model, pos),
					$elm$core$Platform$Cmd$none);
			default:
				return A3(
					$author$project$Utils$logError,
					'mouseMove',
					'Received \"Move\" message when dragState is ' + $author$project$Utils$toString(model.mouse.dragState),
					_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
		}
	});
var $author$project$MouseAPI$mouseOut = F4(
	function (model, _class, targetId, targetMapId) {
		var _v0 = model.mouse.dragState;
		if (_v0.$ === 'Drag') {
			var dragMode = _v0.a;
			var id = _v0.b;
			var mapId = _v0.c;
			var origPos = _v0.d;
			var lastPos = _v0.e;
			return A2(
				$author$project$MouseAPI$updateDragState,
				model,
				A6($author$project$Mouse$Drag, dragMode, id, mapId, origPos, lastPos, $elm$core$Maybe$Nothing));
		} else {
			return model;
		}
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$MouseAPI$mouseOver = F4(
	function (model, _class, targetId, targetMapId) {
		var _v0 = model.mouse.dragState;
		switch (_v0.$) {
			case 'Drag':
				var dragMode = _v0.a;
				var id = _v0.b;
				var mapId = _v0.c;
				var origPos = _v0.d;
				var lastPos = _v0.e;
				var target = (!_Utils_eq(
					_Utils_Tuple2(id, mapId),
					_Utils_Tuple2(targetId, targetMapId))) ? $elm$core$Maybe$Just(
					_Utils_Tuple2(targetId, targetMapId)) : $elm$core$Maybe$Nothing;
				return A2(
					$author$project$MouseAPI$updateDragState,
					model,
					A6($author$project$Mouse$Drag, dragMode, id, mapId, origPos, lastPos, target));
			case 'DragEngaged':
				return A3($author$project$Utils$logError, 'mouseOver', 'Received \"Over\" message when dragState is DragEngaged', model);
			default:
				return model;
		}
	});
var $author$project$AppModel$MoveTopicToMap = F6(
	function (a, b, c, d, e, f) {
		return {$: 'MoveTopicToMap', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $author$project$ModelAPI$createAssocAndAddToMap = F7(
	function (itemType, player1, role1, player2, role2, mapId, model) {
		var props = $author$project$Model$MapAssoc($author$project$Model$AssocProps);
		var _v0 = A6($author$project$ModelAPI$createAssoc, itemType, player1, role1, player2, role2, model);
		var newModel = _v0.a;
		var assocId = _v0.b;
		return A4($author$project$ModelAPI$addItemToMap, assocId, props, mapId, newModel);
	});
var $author$project$ModelAPI$createDefaultAssoc = F4(
	function (player1, player2, mapId, model) {
		return A7($author$project$ModelAPI$createAssocAndAddToMap, 'dmx.association', player1, 'dmx.default', player2, 'dmx.default', mapId, model);
	});
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0.a;
		var genB = _v1.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v2 = genA(seed0);
				var a = _v2.a;
				var seed1 = _v2.b;
				var _v3 = genB(seed1);
				var b = _v3.a;
				var seed2 = _v3.b;
				return _Utils_Tuple2(
					A2(func, a, b),
					seed2);
			});
	});
var $author$project$Config$whiteBoxRange = A2($author$project$Model$Size, 250, 150);
var $author$project$MouseAPI$point = function () {
	var rw = $author$project$Config$whiteBoxRange.w;
	var rh = $author$project$Config$whiteBoxRange.h;
	var cy = $author$project$Config$topicH2 + $author$project$Config$whiteBoxPadding;
	var cx = $author$project$Config$topicW2 + $author$project$Config$whiteBoxPadding;
	return A3(
		$elm$random$Random$map2,
		F2(
			function (x, y) {
				return A2($author$project$Model$Point, cx + x, cy + y);
			}),
		A2($elm$random$Random$float, 0, rw),
		A2($elm$random$Random$float, 0, rh));
}();
var $author$project$MouseAPI$mouseUp = function (model) {
	var _v0 = function () {
		var _v1 = model.mouse.dragState;
		_v1$2:
		while (true) {
			switch (_v1.$) {
				case 'Drag':
					if (_v1.a.$ === 'DragTopic') {
						if (_v1.f.$ === 'Just') {
							var _v2 = _v1.a;
							var id = _v1.b;
							var mapId = _v1.c;
							var origPos = _v1.d;
							var _v3 = _v1.f.a;
							var targetId = _v3.a;
							var targetMapId = _v3.b;
							var notDroppedOnOwnMap = !_Utils_eq(mapId, targetId);
							var msg = A5($author$project$AppModel$MoveTopicToMap, id, mapId, origPos, targetId, targetMapId);
							var _v4 = A2(
								$author$project$Utils$info,
								'mouseUp',
								'dropped ' + ($elm$core$String$fromInt(id) + (' (map ' + ($elm$core$String$fromInt(mapId) + (') on ' + ($elm$core$String$fromInt(targetId) + (' (map ' + ($elm$core$String$fromInt(targetMapId) + (') --> ' + (notDroppedOnOwnMap ? 'move topic' : 'abort'))))))))));
							return notDroppedOnOwnMap ? _Utils_Tuple2(
								model,
								A2($elm$random$Random$generate, msg, $author$project$MouseAPI$point)) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						} else {
							break _v1$2;
						}
					} else {
						if (_v1.f.$ === 'Just') {
							var _v5 = _v1.a;
							var id = _v1.b;
							var mapId = _v1.c;
							var _v6 = _v1.f.a;
							var targetId = _v6.a;
							var targetMapId = _v6.b;
							var isSameMap = _Utils_eq(mapId, targetMapId);
							var _v7 = A2(
								$author$project$Utils$info,
								'mouseUp',
								'assoc drawn from ' + ($elm$core$String$fromInt(id) + (' (map ' + ($elm$core$String$fromInt(mapId) + (') to ' + ($elm$core$String$fromInt(targetId) + (' (map ' + ($elm$core$String$fromInt(targetMapId) + (') --> ' + (isSameMap ? 'create assoc' : 'abort'))))))))));
							return isSameMap ? _Utils_Tuple2(
								A4($author$project$ModelAPI$createDefaultAssoc, id, targetId, mapId, model),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						} else {
							break _v1$2;
						}
					}
				case 'DragEngaged':
					var _v9 = A2($author$project$Utils$info, 'mouseUp', 'drag aborted w/o moving');
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				default:
					return A3(
						$author$project$Utils$logError,
						'mouseUp',
						'Received \"Up\" message when dragState is ' + $author$project$Utils$toString(model.mouse.dragState),
						_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
			}
		}
		var id = _v1.b;
		var mapId = _v1.c;
		var _v8 = A2($author$project$Utils$info, 'mouseUp', 'drag ended w/o target');
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	}();
	var newModel = _v0.a;
	var cmd = _v0.b;
	return _Utils_Tuple2(
		A2($author$project$MouseAPI$updateDragState, newModel, $author$project$Mouse$NoDrag),
		cmd);
};
var $author$project$Mouse$DragEngaged = F5(
	function (a, b, c, d, e) {
		return {$: 'DragEngaged', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Mouse$DragTopic = {$: 'DragTopic'};
var $author$project$Mouse$DrawAssoc = {$: 'DrawAssoc'};
var $author$project$Config$assocDelayMillis = 200;
var $author$project$ModelAPI$getTopicPos = F3(
	function (topicId, mapId, maps) {
		var _v0 = A3($author$project$ModelAPI$getTopicProps, topicId, mapId, maps);
		if (_v0.$ === 'Just') {
			var pos = _v0.a.pos;
			return $elm$core$Maybe$Just(pos);
		} else {
			return A3(
				$author$project$Utils$fail,
				'getTopicPos',
				{mapId: mapId, topicId: topicId},
				$elm$core$Maybe$Nothing);
		}
	});
var $author$project$MouseAPI$timeArrived = F2(
	function (time, model) {
		var _v0 = model.mouse.dragState;
		switch (_v0.$) {
			case 'WaitForStartTime':
				var _class = _v0.a;
				var id = _v0.b;
				var mapId = _v0.c;
				var pos = _v0.d;
				return A2(
					$author$project$MouseAPI$updateDragState,
					model,
					A5($author$project$Mouse$DragEngaged, time, _class, id, mapId, pos));
			case 'WaitForEndTime':
				var startTime = _v0.a;
				var _class = _v0.b;
				var id = _v0.c;
				var mapId = _v0.d;
				var pos = _v0.e;
				return A2(
					$author$project$MouseAPI$updateDragState,
					model,
					function () {
						if (_class === 'dmx-topic') {
							var origPos_ = A3($author$project$ModelAPI$getTopicPos, id, mapId, model.maps);
							var delay = _Utils_cmp(
								$elm$time$Time$posixToMillis(time) - $elm$time$Time$posixToMillis(startTime),
								$author$project$Config$assocDelayMillis) > 0;
							var dragMode = delay ? $author$project$Mouse$DrawAssoc : $author$project$Mouse$DragTopic;
							if (origPos_.$ === 'Just') {
								var origPos = origPos_.a;
								return A6($author$project$Mouse$Drag, dragMode, id, mapId, origPos, pos, $elm$core$Maybe$Nothing);
							} else {
								return $author$project$Mouse$NoDrag;
							}
						} else {
							return $author$project$Mouse$NoDrag;
						}
					}());
			default:
				return A3($author$project$Utils$logError, 'timeArrived', 'Received \"Time\" message when dragState is not WaitForTime', model);
		}
	});
var $author$project$MouseAPI$updateMouse = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Down':
				return _Utils_Tuple2(
					$author$project$MouseAPI$mouseDown(model),
					$elm$core$Platform$Cmd$none);
			case 'DownItem':
				var _class = msg.a;
				var id = msg.b;
				var mapId = msg.c;
				var pos = msg.d;
				return A5($author$project$MouseAPI$mouseDownOnItem, model, _class, id, mapId, pos);
			case 'Move':
				var pos = msg.a;
				return A2($author$project$MouseAPI$mouseMove, model, pos);
			case 'Up':
				return $author$project$Storage$storeModelWith(
					$author$project$MouseAPI$mouseUp(model));
			case 'Over':
				var _class = msg.a;
				var id = msg.b;
				var mapId = msg.c;
				return _Utils_Tuple2(
					A4($author$project$MouseAPI$mouseOver, model, _class, id, mapId),
					$elm$core$Platform$Cmd$none);
			case 'Out':
				var _class = msg.a;
				var id = msg.b;
				var mapId = msg.c;
				return _Utils_Tuple2(
					A4($author$project$MouseAPI$mouseOut, model, _class, id, mapId),
					$elm$core$Platform$Cmd$none);
			default:
				var time = msg.a;
				return _Utils_Tuple2(
					A2($author$project$MouseAPI$timeArrived, time, model),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$ModelAPI$updateMapRect = F3(
	function (mapId, rectFunc, model) {
		return _Utils_update(
			model,
			{
				maps: A3(
					$author$project$ModelAPI$updateMaps,
					mapId,
					function (map) {
						return _Utils_update(
							map,
							{
								rect: rectFunc(map.rect)
							});
					},
					model.maps)
			});
	});
var $author$project$Main$adjustMapRect = F3(
	function (mapId, factor, model) {
		return A3(
			$author$project$ModelAPI$updateMapRect,
			mapId,
			function (rect) {
				return A4($author$project$Model$Rectangle, rect.x1 + (factor * 400), rect.y1 + (factor * 300), rect.x2, rect.y2);
			},
			model);
	});
var $author$project$Main$back = function (model) {
	var _v0 = function () {
		var _v1 = model.mapPath;
		if (_v1.b && _v1.b.b) {
			var prevMapId = _v1.a;
			var _v2 = _v1.b;
			var nextMapId = _v2.a;
			var mapIds = _v2.b;
			return _Utils_Tuple3(
				prevMapId,
				A2($elm$core$List$cons, nextMapId, mapIds),
				_List_fromArray(
					[
						_Utils_Tuple2(prevMapId, nextMapId)
					]));
		} else {
			return A3(
				$author$project$Utils$logError,
				'back',
				'model.mapPath has a problem',
				_Utils_Tuple3(
					0,
					_List_fromArray(
						[0]),
					_List_Nil));
		}
	}();
	var mapId = _v0.a;
	var mapPath = _v0.b;
	var selection = _v0.c;
	return $author$project$MapAutoSize$autoSize(
		A3(
			$author$project$Main$adjustMapRect,
			mapId,
			1,
			_Utils_update(
				model,
				{mapPath: mapPath, selection: selection})));
};
var $author$project$Main$fullscreen = function (model) {
	var _v0 = $author$project$ModelAPI$getSingleSelection(model);
	if (_v0.$ === 'Just') {
		var _v1 = _v0.a;
		var topicId = _v1.a;
		var mapId = _v1.b;
		return A3(
			$author$project$Main$adjustMapRect,
			topicId,
			-1,
			A3(
				$author$project$Main$createMapIfNeeded,
				topicId,
				mapId,
				_Utils_update(
					model,
					{
						mapPath: A2($elm$core$List$cons, topicId, model.mapPath),
						selection: _List_Nil
					})).a);
	} else {
		return model;
	}
};
var $author$project$Main$updateNav = F2(
	function (navMsg, model) {
		if (navMsg.$ === 'Fullscreen') {
			return $author$project$Main$fullscreen(model);
		} else {
			return $author$project$Main$back(model);
		}
	});
var $author$project$Search$Open = function (a) {
	return {$: 'Open', a: a};
};
var $author$project$SearchAPI$onFocusInput = function (model) {
	var search = model.search;
	return _Utils_update(
		model,
		{
			search: _Utils_update(
				search,
				{
					menu: $author$project$Search$Open($elm$core$Maybe$Nothing)
				})
		});
};
var $author$project$SearchAPI$onHoverItem = F2(
	function (topicId, model) {
		var search = model.search;
		var _v0 = model.search.menu;
		if (_v0.$ === 'Open') {
			return _Utils_update(
				model,
				{
					search: _Utils_update(
						search,
						{
							menu: $author$project$Search$Open(
								$elm$core$Maybe$Just(topicId))
						})
				});
		} else {
			return A3($author$project$Utils$logError, 'onHoverItem', 'Received \"HoverItem\" message when search.menu is Closed', model);
		}
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$SearchAPI$isMatch = F2(
	function (searchText, text) {
		return (!$elm$core$String$isEmpty(searchText)) && A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(searchText),
			$elm$core$String$toLower(text));
	});
var $author$project$SearchAPI$searchTopics = function (model) {
	var search = model.search;
	return _Utils_update(
		model,
		{
			search: _Utils_update(
				search,
				{
					menu: $author$project$Search$Open($elm$core$Maybe$Nothing),
					result: A3(
						$elm$core$Dict$foldr,
						F3(
							function (id, item, topicIds) {
								if (item.$ === 'Topic') {
									var text = item.a.text;
									return A2($author$project$SearchAPI$isMatch, model.search.text, text) ? A2($elm$core$List$cons, id, topicIds) : topicIds;
								} else {
									return topicIds;
								}
							}),
						_List_Nil,
						model.items)
				})
		});
};
var $author$project$SearchAPI$onTextInput = F2(
	function (text, model) {
		var search = model.search;
		return $author$project$SearchAPI$searchTopics(
			_Utils_update(
				model,
				{
					search: _Utils_update(
						search,
						{text: text})
				}));
	});
var $author$project$SearchAPI$onUnhoverItem = function (model) {
	var search = model.search;
	var _v0 = model.search.menu;
	if (_v0.$ === 'Open') {
		return _Utils_update(
			model,
			{
				search: _Utils_update(
					search,
					{
						menu: $author$project$Search$Open($elm$core$Maybe$Nothing)
					})
			});
	} else {
		return A3($author$project$Utils$logError, 'onUnhoverItem', 'Received \"UnhoverItem\" message when search.menu is Closed', model);
	}
};
var $author$project$ModelAPI$defaultProps = F3(
	function (topicId, size, model) {
		return A3(
			$author$project$Model$TopicProps,
			A2($author$project$Model$Point, 0, 0),
			size,
			A2($author$project$ModelAPI$hasMap, topicId, model.maps) ? $author$project$Model$Container($author$project$Model$BlackBox) : $author$project$Model$Monad($author$project$Model$LabelOnly));
	});
var $author$project$ModelAPI$isItemInMap = F3(
	function (itemId, mapId, model) {
		var _v0 = A2($author$project$ModelAPI$getMap, mapId, model.maps);
		if (_v0.$ === 'Just') {
			var map = _v0.a;
			var _v1 = A2($elm$core$Dict$get, itemId, map.items);
			if (_v1.$ === 'Just') {
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var $author$project$ModelAPI$showItem = F3(
	function (itemId, mapId, model) {
		return _Utils_update(
			model,
			{
				maps: A3(
					$author$project$ModelAPI$updateMaps,
					mapId,
					function (map) {
						return _Utils_update(
							map,
							{
								items: A3(
									$elm$core$Dict$update,
									itemId,
									function (maybeItem) {
										if (maybeItem.$ === 'Just') {
											var mapItem = maybeItem.a;
											return $elm$core$Maybe$Just(
												_Utils_update(
													mapItem,
													{hidden: false}));
										} else {
											return $elm$core$Maybe$Nothing;
										}
									},
									map.items)
							});
					},
					model.maps)
			});
	});
var $author$project$SearchAPI$revealTopic = F3(
	function (topicId, mapId, model) {
		if (A3($author$project$ModelAPI$isItemInMap, topicId, mapId, model)) {
			var _v0 = A2(
				$author$project$Utils$info,
				'revealTopic',
				_Utils_Tuple2(topicId, 'set visible'));
			return A3($author$project$ModelAPI$showItem, topicId, mapId, model);
		} else {
			var props = $author$project$Model$MapTopic(
				A3($author$project$ModelAPI$defaultProps, topicId, $author$project$Config$topicSize, model));
			var _v1 = A2(
				$author$project$Utils$info,
				'revealTopic',
				_Utils_Tuple2(topicId, 'add to map'));
			return A4($author$project$ModelAPI$addItemToMap, topicId, props, mapId, model);
		}
	});
var $author$project$SearchAPI$updateSearch = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Input':
				var text = msg.a;
				return _Utils_Tuple2(
					A2($author$project$SearchAPI$onTextInput, text, model),
					$elm$core$Platform$Cmd$none);
			case 'FocusInput':
				return _Utils_Tuple2(
					$author$project$SearchAPI$onFocusInput(model),
					$elm$core$Platform$Cmd$none);
			case 'HoverItem':
				var topicId = msg.a;
				return _Utils_Tuple2(
					A2($author$project$SearchAPI$onHoverItem, topicId, model),
					$elm$core$Platform$Cmd$none);
			case 'UnhoverItem':
				return _Utils_Tuple2(
					$author$project$SearchAPI$onUnhoverItem(model),
					$elm$core$Platform$Cmd$none);
			default:
				var topicId = msg.a;
				return $author$project$Storage$storeModel(
					$author$project$SearchAPI$closeResultMenu(
						A3(
							$author$project$SearchAPI$revealTopic,
							topicId,
							$author$project$ModelAPI$activeMap(model),
							model)));
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = function () {
			if (msg.$ === 'Mouse') {
				return msg;
			} else {
				return A2($author$project$Utils$info, 'update', msg);
			}
		}();
		switch (msg.$) {
			case 'AddTopic':
				return $author$project$Storage$storeModel(
					A4(
						$author$project$ModelAPI$createTopicAndAddToMap,
						$author$project$Config$topicDefaultText,
						$elm$core$Maybe$Nothing,
						$author$project$ModelAPI$activeMap(model),
						model));
			case 'MoveTopicToMap':
				var topicId = msg.a;
				var mapId = msg.b;
				var origPos = msg.c;
				var targetId = msg.d;
				var targetMapId = msg.e;
				var pos = msg.f;
				return $author$project$Storage$storeModel(
					A7($author$project$Main$moveTopicToMap, topicId, mapId, origPos, targetId, targetMapId, pos, model));
			case 'SwitchDisplay':
				var displayMode = msg.a;
				return $author$project$Storage$storeModel(
					A2($author$project$Main$switchDisplay, displayMode, model));
			case 'Search':
				var searchMsg = msg.a;
				return A2($author$project$SearchAPI$updateSearch, searchMsg, model);
			case 'Edit':
				var editMsg = msg.a;
				return A2($author$project$Main$updateEdit, editMsg, model);
			case 'IconMenu':
				var iconMenuMsg = msg.a;
				return A2($author$project$IconMenuAPI$updateIconMenu, iconMenuMsg, model);
			case 'Mouse':
				var mouseMsg = msg.a;
				return A2($author$project$MouseAPI$updateMouse, mouseMsg, model);
			case 'Nav':
				var navMsg = msg.a;
				return $author$project$Storage$storeModel(
					A2($author$project$Main$updateNav, navMsg, model));
			case 'Hide':
				return $author$project$Storage$storeModel(
					$author$project$Main$hide(model));
			case 'Delete':
				return $author$project$Storage$storeModel(
					$author$project$Main$delete(model));
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $elm$browser$Browser$Document = F2(
	function (title, body) {
		return {body: body, title: title};
	});
var $author$project$Config$mainFont = 'sans-serif';
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$appStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'font-family', $author$project$Config$mainFont),
		A2($elm$html$Html$Attributes$style, 'user-select', 'none'),
		A2($elm$html$Html$Attributes$style, '-webkit-user-select', 'none')
	]);
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Main$measureStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
		A2($elm$html$Html$Attributes$style, 'visibility', 'hidden'),
		A2($elm$html$Html$Attributes$style, 'white-space', 'pre-wrap'),
		A2($elm$html$Html$Attributes$style, 'font-family', $author$project$Config$mainFont),
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'line-height',
		$elm$core$String$fromFloat($author$project$Config$topicLineHeight)),
		A2(
		$elm$html$Html$Attributes$style,
		'padding',
		$elm$core$String$fromInt($author$project$Config$topicDetailPadding) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromFloat($author$project$Config$topicDetailMaxWidth) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'min-width',
		$elm$core$String$fromFloat($author$project$Config$topicSize.w - $author$project$Config$topicSize.h) + 'px'),
		A2($elm$html$Html$Attributes$style, 'max-width', 'max-content'),
		A2(
		$elm$html$Html$Attributes$style,
		'border-width',
		$elm$core$String$fromFloat($author$project$Config$topicBorderWidth) + 'px'),
		A2($elm$html$Html$Attributes$style, 'border-style', 'solid'),
		A2($elm$html$Html$Attributes$style, 'box-sizing', 'border-box')
	]);
var $author$project$Mouse$Out = F3(
	function (a, b, c) {
		return {$: 'Out', a: a, b: b, c: c};
	});
var $author$project$Mouse$Over = F3(
	function (a, b, c) {
		return {$: 'Over', a: a, b: b, c: c};
	});
var $author$project$MouseAPI$mouseDecoder = function (msg) {
	return A2(
		$elm$json$Json$Decode$map,
		$author$project$AppModel$Mouse,
		A4(
			$elm$json$Json$Decode$map3,
			msg,
			$elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['target', 'className']),
						$elm$json$Json$Decode$string),
						A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['target', 'className', 'baseVal']),
						$elm$json$Json$Decode$string)
					])),
			A2(
				$elm$json$Json$Decode$andThen,
				$author$project$Utils$strToIntDecoder,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['target', 'dataset', 'id']),
					$elm$json$Json$Decode$string)),
			A2(
				$elm$json$Json$Decode$andThen,
				$author$project$Utils$strToIntDecoder,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['target', 'dataset', 'mapId']),
					$elm$json$Json$Decode$string))));
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $author$project$MouseAPI$mouseHoverHandler = _List_fromArray(
	[
		A2(
		$elm$html$Html$Events$on,
		'mouseover',
		$author$project$MouseAPI$mouseDecoder($author$project$Mouse$Over)),
		A2(
		$elm$html$Html$Events$on,
		'mouseout',
		$author$project$MouseAPI$mouseDecoder($author$project$Mouse$Out))
	]);
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$IconMenu$Close = {$: 'Close'};
var $author$project$AppModel$IconMenu = function (a) {
	return {$: 'IconMenu', a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$IconMenuAPI$closeButtonStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '0'),
		A2($elm$html$Html$Attributes$style, 'right', '0')
	]);
var $author$project$IconMenuAPI$iconListStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'height', '100%'),
		A2($elm$html$Html$Attributes$style, 'overflow', 'auto')
	]);
var $author$project$IconMenuAPI$iconMenuStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '291px'),
		A2($elm$html$Html$Attributes$style, 'width', '320px'),
		A2($elm$html$Html$Attributes$style, 'height', '320px'),
		A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
		A2($elm$html$Html$Attributes$style, 'border', '1px solid lightgray'),
		A2($elm$html$Html$Attributes$style, 'z-index', '1')
	]);
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$svg$Svg$map = $elm$virtual_dom$VirtualDom$map;
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $feathericons$elm_feather$FeatherIcons$toHtml = F2(
	function (attributes, _v0) {
		var src = _v0.a.src;
		var attrs = _v0.a.attrs;
		var strSize = $elm$core$String$fromFloat(attrs.size);
		var baseAttributes = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$fill('none'),
				$elm$svg$Svg$Attributes$height(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$width(
				_Utils_ap(strSize, attrs.sizeUnit)),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeLinecap('round'),
				$elm$svg$Svg$Attributes$strokeLinejoin('round'),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(attrs.strokeWidth)),
				$elm$svg$Svg$Attributes$viewBox(attrs.viewBox)
			]);
		var combinedAttributes = _Utils_ap(
			function () {
				var _v1 = attrs._class;
				if (_v1.$ === 'Just') {
					var c = _v1.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$class(c),
						baseAttributes);
				} else {
					return baseAttributes;
				}
			}(),
			attributes);
		return A2(
			$elm$svg$Svg$svg,
			combinedAttributes,
			A2(
				$elm$core$List$map,
				$elm$svg$Svg$map($elm$core$Basics$never),
				src));
	});
var $author$project$IconMenu$SetIcon = function (a) {
	return {$: 'SetIcon', a: a};
};
var $author$project$IconMenuAPI$iconButtonStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'border-width', '0'),
		A2($elm$html$Html$Attributes$style, 'margin', '8px')
	]);
var $feathericons$elm_feather$FeatherIcons$Icon = function (a) {
	return {$: 'Icon', a: a};
};
var $feathericons$elm_feather$FeatherIcons$defaultAttributes = function (name) {
	return {
		_class: $elm$core$Maybe$Just('feather feather-' + name),
		size: 24,
		sizeUnit: '',
		strokeWidth: 2,
		viewBox: '0 0 24 24'
	};
};
var $feathericons$elm_feather$FeatherIcons$makeBuilder = F2(
	function (name, src) {
		return $feathericons$elm_feather$FeatherIcons$Icon(
			{
				attrs: $feathericons$elm_feather$FeatherIcons$defaultAttributes(name),
				src: src
			});
	});
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $feathericons$elm_feather$FeatherIcons$activity = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'activity',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 12 18 12 15 21 9 3 6 12 2 12')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $feathericons$elm_feather$FeatherIcons$airplay = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'airplay',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 17H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2h-1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 15 17 21 7 21 12 15')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $feathericons$elm_feather$FeatherIcons$alertCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'alert-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alertOctagon = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'alert-octagon',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alertTriangle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'alert-triangle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alignCenter = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'align-center',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alignJustify = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'align-justify',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alignLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'align-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$alignRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'align-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$anchor = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'anchor',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('5'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 12H2a10 10 0 0 0 20 0h-3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$aperture = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'aperture',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.31'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('20.05'),
					$elm$svg$Svg$Attributes$y2('17.94')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9.69'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('21.17'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('7.38'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('13.12'),
					$elm$svg$Svg$Attributes$y2('2.06')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9.69'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('3.95'),
					$elm$svg$Svg$Attributes$y2('6.06')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.31'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('2.83'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16.62'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('10.88'),
					$elm$svg$Svg$Attributes$y2('21.94')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $feathericons$elm_feather$FeatherIcons$archive = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'archive',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('21 8 21 21 3 21 3 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('22'),
					$elm$svg$Svg$Attributes$height('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('5'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('19 12 12 19 5 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowDownCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-down-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 12 12 16 16 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowDownLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-down-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('7'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 17 7 17 7 7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowDownRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-down-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('7'),
					$elm$svg$Svg$Attributes$y1('7'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 7 17 17 7 17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('19'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('5'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 19 5 12 12 5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowLeftCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-left-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 8 8 12 12 16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('19'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 5 19 12 12 19')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowRightCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-right-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 16 16 12 12 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('5 12 12 5 19 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowUpCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-up-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 12 12 8 8 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowUpLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-up-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 17 7 7 17 7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$arrowUpRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'arrow-up-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('7'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 7 17 7 17 17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$atSign = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'at-sign',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 8v5a3 3 0 0 0 6 0v-1a10 10 0 1 0-3.92 7.94')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$award = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'award',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('8'),
					$elm$svg$Svg$Attributes$r('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8.21 13.89 7 23 12 20 17 23 15.79 13.88')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$barChart = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bar-chart',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$barChart2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bar-chart-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $feathericons$elm_feather$FeatherIcons$battery = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'battery',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('6'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('12'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$batteryCharging = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'battery-charging',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 18H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h3.19M15 6h2a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2h-3.19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 6 7 12 13 12 9 18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bell = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bell',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M13.73 21a2 2 0 0 1-3.46 0')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bellOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bell-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M13.73 21a2 2 0 0 1-3.46 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18.63 13A17.89 17.89 0 0 1 18 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6.26 6.26A5.86 5.86 0 0 0 6 8c0 7-3 9-3 9h14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 8a6 6 0 0 0-9.33-5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bluetooth = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bluetooth',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('6.5 6.5 17.5 17.5 12 23 12 1 17.5 6.5 6.5 17.5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bold = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bold',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 4h8a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 12h9a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$book = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'book',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 19.5A2.5 2.5 0 0 1 6.5 17H20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bookOpen = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'book-open',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$bookmark = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'bookmark',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$box = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'box',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3.27 6.96 12 12.01 20.73 6.96')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22.08'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$briefcase = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'briefcase',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('7'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('14'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$calendar = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'calendar',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('4'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$camera = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'camera',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('13'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cameraOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'camera-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 21H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h3m3-3h6l2 3h4a2 2 0 0 1 2 2v9.34m-7.72-2.06a4 4 0 1 1-5.56-5.56')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cast = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cast',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2 16.1A5 5 0 0 1 5.9 20M2 12.05A9 9 0 0 1 9.95 20M2 8V6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2h-6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('2.01'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$check = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'check',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('20 6 9 17 4 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$checkCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'check-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 11.08V12a10 10 0 1 1-5.93-9.14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 4 12 14.01 9 11.01')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$checkSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'check-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 11 12 14 22 4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevron-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('6 9 12 15 18 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevron-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 18 9 12 15 6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevron-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 18 15 12 9 6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevron-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('18 15 12 9 6 15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronsDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevrons-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 13 12 18 17 13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 6 12 11 17 6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronsLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevrons-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 17 6 12 11 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('18 17 13 12 18 7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronsRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevrons-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('13 17 18 12 13 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('6 17 11 12 6 7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chevronsUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chevrons-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 11 12 6 7 11')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 18 12 13 7 18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$chrome = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'chrome',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21.17'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3.95'),
					$elm$svg$Svg$Attributes$y1('6.06'),
					$elm$svg$Svg$Attributes$x2('8.54'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10.88'),
					$elm$svg$Svg$Attributes$y1('21.94'),
					$elm$svg$Svg$Attributes$x2('15.46'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$circle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$clipboard = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'clipboard',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('8'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('8'),
					$elm$svg$Svg$Attributes$height('4'),
					$elm$svg$Svg$Attributes$rx('1'),
					$elm$svg$Svg$Attributes$ry('1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$clock = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'clock',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 6 12 12 16 14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloud = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 10h-1.26A8 8 0 1 0 9 20h9a5 5 0 0 0 0-10z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloudDrizzle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud-drizzle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 16.58A5 5 0 0 0 18 7h-1.26A8 8 0 1 0 4 15.25')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloudLightning = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud-lightning',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 16.9A5 5 0 0 0 18 7h-1.26a8 8 0 1 0-11.62 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('13 11 9 17 15 17 11 23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloudOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22.61 16.95A5 5 0 0 0 18 10h-1.26a8 8 0 0 0-7.05-6M5 5a8 8 0 0 0 4 15h9a5 5 0 0 0 1.7-.3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloudRain = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud-rain',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 16.58A5 5 0 0 0 18 7h-1.26A8 8 0 1 0 4 15.25')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cloudSnow = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cloud-snow',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 17.58A5 5 0 0 0 18 8h-1.26A8 8 0 1 0 4 16.25')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('8.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('8.01'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('16.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('16.01'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$code = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'code',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 18 22 12 16 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 6 2 12 8 18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$codepen = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'codepen',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 2 22 8.5 22 15.5 12 22 2 15.5 2 8.5 12 2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('15.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 8.5 12 15.5 2 8.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('2 15.5 12 8.5 22 15.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8.5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$codesandbox = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'codesandbox',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7.5 4.21 12 6.81 16.5 4.21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7.5 19.79 7.5 14.6 3 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('21 12 16.5 14.6 16.5 19.79')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3.27 6.96 12 12.01 20.73 6.96')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22.08'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$coffee = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'coffee',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 8h1a4 4 0 0 1 0 8h-1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2 8h16v9a4 4 0 0 1-4 4H6a4 4 0 0 1-4-4V8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$columns = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'columns',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 3h7a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-7m0-18H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h7m0-18v18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$command = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'command',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 3a3 3 0 0 0-3 3v12a3 3 0 0 0 3 3 3 3 0 0 0 3-3 3 3 0 0 0-3-3H6a3 3 0 0 0-3 3 3 3 0 0 0 3 3 3 3 0 0 0 3-3V6a3 3 0 0 0-3-3 3 3 0 0 0-3 3 3 3 0 0 0 3 3h12a3 3 0 0 0 3-3 3 3 0 0 0-3-3z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$compass = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'compass',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16.24 7.76 14.12 14.12 7.76 16.24 9.88 9.88 16.24 7.76')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$copy = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'copy',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('9'),
					$elm$svg$Svg$Attributes$y('9'),
					$elm$svg$Svg$Attributes$width('13'),
					$elm$svg$Svg$Attributes$height('13'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerDownLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-down-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 10 4 15 9 20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 4v7a4 4 0 0 1-4 4H4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerDownRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-down-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 10 20 15 15 20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 4v7a4 4 0 0 0 4 4h12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerLeftDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-left-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 15 9 20 4 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 4h-7a4 4 0 0 0-4 4v12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerLeftUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-left-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 9 9 4 4 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 20h-7a4 4 0 0 1-4-4V4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerRightDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-right-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 15 15 20 20 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 4h7a4 4 0 0 1 4 4v12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerRightUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-right-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 9 15 4 20 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 20h7a4 4 0 0 0 4-4V4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerUpLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-up-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 14 4 9 9 4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 20v-7a4 4 0 0 0-4-4H4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cornerUpRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'corner-up-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 14 20 9 15 4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 20v-7a4 4 0 0 1 4-4h12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$cpu = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'cpu',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('4'),
					$elm$svg$Svg$Attributes$y('4'),
					$elm$svg$Svg$Attributes$width('16'),
					$elm$svg$Svg$Attributes$height('16'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('9'),
					$elm$svg$Svg$Attributes$y('9'),
					$elm$svg$Svg$Attributes$width('6'),
					$elm$svg$Svg$Attributes$height('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('4'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('4'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$creditCard = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'credit-card',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('4'),
					$elm$svg$Svg$Attributes$width('22'),
					$elm$svg$Svg$Attributes$height('16'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$crop = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'crop',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6.13 1L6 16a2 2 0 0 0 2 2h15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M1 6.13L16 6a2 2 0 0 1 2 2v15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$crosshair = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'crosshair',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('22'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('2'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$ellipse = $elm$svg$Svg$trustedNode('ellipse');
var $feathericons$elm_feather$FeatherIcons$database = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'database',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$ellipse,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('5'),
					$elm$svg$Svg$Attributes$rx('9'),
					$elm$svg$Svg$Attributes$ry('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 12c0 1.66-4 3-9 3s-9-1.34-9-3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$delete = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'delete',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 4H8l-7 8 7 8h13a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$disc = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'disc',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$divide = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'divide',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('6'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('19'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$divideCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'divide-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$divideSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'divide-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$dollarSign = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'dollar-sign',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$download = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'download',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 10 12 15 17 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$downloadCloud = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'download-cloud',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 17 12 21 16 17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.88 18.09A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.29')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$dribbble = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'dribbble',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8.56 2.75c4.37 6.03 6.02 9.42 8.03 17.72m2.54-15.38c-3.72 4.35-8.94 5.66-16.88 5.85m19.5 1.9c-3.5-.93-6.63-.82-8.94 0-2.58.92-5.01 2.86-7.44 6.32')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$droplet = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'droplet',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 2.69l5.66 5.66a8 8 0 1 1-11.31 0z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$edit = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'edit',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$edit2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'edit-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$edit3 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'edit-3',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 20h9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$externalLink = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'external-link',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 3 21 3 21 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$eye = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'eye',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$eyeOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'eye-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$facebook = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'facebook',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 2h-3a5 5 0 0 0-5 5v3H7v4h3v8h4v-8h3l1-4h-4V7a1 1 0 0 1 1-1h3z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$fastForward = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'fast-forward',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('13 19 22 12 13 5 13 19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('2 19 11 12 2 5 2 19')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$feather = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'feather',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.24 12.24a6 6 0 0 0-8.49-8.49L5 10.5V19h8.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('2'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17.5'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$figma = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'figma',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 5.5A3.5 3.5 0 0 1 8.5 2H12v7H8.5A3.5 3.5 0 0 1 5 5.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 2h3.5a3.5 3.5 0 1 1 0 7H12V2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 12.5a3.5 3.5 0 1 1 7 0 3.5 3.5 0 1 1-7 0z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 19.5A3.5 3.5 0 0 1 8.5 16H12v3.5a3.5 3.5 0 1 1-7 0z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 12.5A3.5 3.5 0 0 1 8.5 9H12v7H8.5A3.5 3.5 0 0 1 5 12.5z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$file = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'file',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M13 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('13 2 13 9 20 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$fileMinus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'file-minus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 2 14 8 20 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$filePlus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'file-plus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 2 14 8 20 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$fileText = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'file-text',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('14 2 14 8 20 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('13'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 9 9 9 8 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$film = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'film',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('20'),
					$elm$svg$Svg$Attributes$rx('2.18'),
					$elm$svg$Svg$Attributes$ry('2.18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('7'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('7'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('7'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$filter = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'filter',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$flag = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'flag',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('4'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$folder = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'folder',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$folderMinus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'folder-minus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$folderPlus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'folder-plus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$framer = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'framer',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 16V9h14V2H5l14 14h-7m-7 0l7 7v-7m-7 0h7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$frown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'frown',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 16s-1.5-2-4-2-4 2-4 2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gift = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'gift',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('20 12 20 22 4 22 4 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('7'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 7H7.5a2.5 2.5 0 0 1 0-5C11 2 12 7 12 7z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 7h4.5a2.5 2.5 0 0 0 0-5C13 2 12 7 12 7z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gitBranch = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'git-branch',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('6'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 9a9 9 0 0 1-9 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gitCommit = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'git-commit',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1.05'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17.01'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('22.96'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gitMerge = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'git-merge',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('6'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 21V9a9 9 0 0 0 9 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gitPullRequest = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'git-pull-request',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('6'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M13 6h3a2 2 0 0 1 2 2v7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$github = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'github',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$gitlab = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'gitlab',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22.65 14.39L12 22.13 1.35 14.39a.84.84 0 0 1-.3-.94l1.22-3.78 2.44-7.51A.42.42 0 0 1 4.82 2a.43.43 0 0 1 .58 0 .42.42 0 0 1 .11.18l2.44 7.49h8.1l2.44-7.51A.42.42 0 0 1 18.6 2a.43.43 0 0 1 .58 0 .42.42 0 0 1 .11.18l2.44 7.51L23 13.45a.84.84 0 0 1-.35.94z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$globe = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'globe',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$grid = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'grid',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('7'),
					$elm$svg$Svg$Attributes$height('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('14'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('7'),
					$elm$svg$Svg$Attributes$height('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('14'),
					$elm$svg$Svg$Attributes$y('14'),
					$elm$svg$Svg$Attributes$width('7'),
					$elm$svg$Svg$Attributes$height('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('14'),
					$elm$svg$Svg$Attributes$width('7'),
					$elm$svg$Svg$Attributes$height('7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$hardDrive = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'hard-drive',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('22'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('2'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5.45 5.11L2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('6.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('10.01'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$hash = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'hash',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$headphones = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'headphones',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 18v-6a9 9 0 0 1 18 0v6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1 2-2h3zM3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$heart = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'heart',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$helpCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'help-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$hexagon = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'hexagon',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$home = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'home',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 22 9 12 15 12 15 22')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$image = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'image',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8.5'),
					$elm$svg$Svg$Attributes$cy('8.5'),
					$elm$svg$Svg$Attributes$r('1.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('21 15 16 10 5 21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$inbox = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'inbox',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 12 16 12 14 15 10 15 8 12 2 12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5.45 5.11L2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$info = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'info',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$instagram = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'instagram',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('20'),
					$elm$svg$Svg$Attributes$rx('5'),
					$elm$svg$Svg$Attributes$ry('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 11.37A4 4 0 1 1 12.63 8 4 4 0 0 1 16 11.37z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17.5'),
					$elm$svg$Svg$Attributes$y1('6.5'),
					$elm$svg$Svg$Attributes$x2('17.51'),
					$elm$svg$Svg$Attributes$y2('6.5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$italic = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'italic',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('19'),
					$elm$svg$Svg$Attributes$y1('4'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('5'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('4'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$key = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'key',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 2l-2 2m-7.61 7.61a5.5 5.5 0 1 1-7.778 7.778 5.5 5.5 0 0 1 7.777-7.777zm0 0L15.5 7.5m0 0l3 3L22 7l-3-3m-3.5 3.5L19 4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$layers = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'layers',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 2 2 7 12 12 22 7 12 2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('2 17 12 22 22 17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('2 12 12 17 22 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$layout = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'layout',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$lifeBuoy = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'life-buoy',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.93'),
					$elm$svg$Svg$Attributes$y1('4.93'),
					$elm$svg$Svg$Attributes$x2('9.17'),
					$elm$svg$Svg$Attributes$y2('9.17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.83'),
					$elm$svg$Svg$Attributes$y1('14.83'),
					$elm$svg$Svg$Attributes$x2('19.07'),
					$elm$svg$Svg$Attributes$y2('19.07')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.83'),
					$elm$svg$Svg$Attributes$y1('9.17'),
					$elm$svg$Svg$Attributes$x2('19.07'),
					$elm$svg$Svg$Attributes$y2('4.93')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.83'),
					$elm$svg$Svg$Attributes$y1('9.17'),
					$elm$svg$Svg$Attributes$x2('18.36'),
					$elm$svg$Svg$Attributes$y2('5.64')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.93'),
					$elm$svg$Svg$Attributes$y1('19.07'),
					$elm$svg$Svg$Attributes$x2('9.17'),
					$elm$svg$Svg$Attributes$y2('14.83')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$link = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'link',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$link2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'link-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M15 7h3a5 5 0 0 1 5 5 5 5 0 0 1-5 5h-3m-6 0H6a5 5 0 0 1-5-5 5 5 0 0 1 5-5h3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$linkedin = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'linkedin',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 8a6 6 0 0 1 6 6v7h-4v-7a2 2 0 0 0-2-2 2 2 0 0 0-2 2v7h-4v-7a6 6 0 0 1 6-6z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('9'),
					$elm$svg$Svg$Attributes$width('4'),
					$elm$svg$Svg$Attributes$height('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('4'),
					$elm$svg$Svg$Attributes$cy('4'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$list = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'list',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('3.01'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('3.01'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('3.01'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$loader = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'loader',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.93'),
					$elm$svg$Svg$Attributes$y1('4.93'),
					$elm$svg$Svg$Attributes$x2('7.76'),
					$elm$svg$Svg$Attributes$y2('7.76')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16.24'),
					$elm$svg$Svg$Attributes$y1('16.24'),
					$elm$svg$Svg$Attributes$x2('19.07'),
					$elm$svg$Svg$Attributes$y2('19.07')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.93'),
					$elm$svg$Svg$Attributes$y1('19.07'),
					$elm$svg$Svg$Attributes$x2('7.76'),
					$elm$svg$Svg$Attributes$y2('16.24')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16.24'),
					$elm$svg$Svg$Attributes$y1('7.76'),
					$elm$svg$Svg$Attributes$x2('19.07'),
					$elm$svg$Svg$Attributes$y2('4.93')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$lock = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'lock',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('11'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('11'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M7 11V7a5 5 0 0 1 10 0v4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$logIn = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'log-in',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 17 15 12 10 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$logOut = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'log-out',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 17 21 12 16 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$mail = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'mail',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22,6 12,13 2,6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$map = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'map',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('8'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$mapPin = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'map-pin',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('10'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$maximize = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'maximize',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$maximize2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'maximize-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 3 21 3 21 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 21 3 21 3 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$meh = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'meh',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$menu = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'menu',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$messageCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'message-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 11.5a8.38 8.38 0 0 1-.9 3.8 8.5 8.5 0 0 1-7.6 4.7 8.38 8.38 0 0 1-3.8-.9L3 21l1.9-5.7a8.38 8.38 0 0 1-.9-3.8 8.5 8.5 0 0 1 4.7-7.6 8.38 8.38 0 0 1 3.8-.9h.5a8.48 8.48 0 0 1 8 8v.5z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$messageSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'message-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$mic = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'mic',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 10v2a7 7 0 0 1-14 0v-2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('23'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$micOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'mic-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9 9v3a3 3 0 0 0 5.12 2.12M15 9.34V4a3 3 0 0 0-5.94-.6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 16.95A7 7 0 0 1 5 12v-2m14 0v2a7 7 0 0 1-.11 1.23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('23'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$minimize = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'minimize',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$minimize2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'minimize-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('4 14 10 14 10 20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('20 10 14 10 14 4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$minus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'minus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('19'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$minusCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'minus-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$minusSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'minus-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$monitor = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'monitor',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('14'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$moon = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'moon',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$moreHorizontal = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'more-horizontal',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('19'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('5'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$moreVertical = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'more-vertical',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('5'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('19'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$mousePointer = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'mouse-pointer',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M13 13l6 6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$move = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'move',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('5 9 2 12 5 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9 5 12 2 15 5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('15 19 12 22 9 19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('19 9 22 12 19 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('2'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('22'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$music = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'music',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9 18V5l12-2v13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('16'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$navigation = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'navigation',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3 11 22 2 13 21 11 13 3 11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$navigation2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'navigation-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 2 19 21 12 17 5 21 12 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$octagon = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'octagon',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$package = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'package',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16.5'),
					$elm$svg$Svg$Attributes$y1('9.4'),
					$elm$svg$Svg$Attributes$x2('7.5'),
					$elm$svg$Svg$Attributes$y2('4.21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3.27 6.96 12 12.01 20.73 6.96')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('22.08'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$paperclip = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'paperclip',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21.44 11.05l-9.19 9.19a6 6 0 0 1-8.49-8.49l9.19-9.19a4 4 0 0 1 5.66 5.66l-9.2 9.19a2 2 0 0 1-2.83-2.83l8.49-8.48')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$pause = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'pause',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('6'),
					$elm$svg$Svg$Attributes$y('4'),
					$elm$svg$Svg$Attributes$width('4'),
					$elm$svg$Svg$Attributes$height('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('14'),
					$elm$svg$Svg$Attributes$y('4'),
					$elm$svg$Svg$Attributes$width('4'),
					$elm$svg$Svg$Attributes$height('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$pauseCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'pause-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$penTool = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'pen-tool',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 19l7-7 3 3-7 7-3-3z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18 13l-1.5-7.5L2 2l3.5 14.5L13 18l5-5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2 2l7.586 7.586')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('11'),
					$elm$svg$Svg$Attributes$cy('11'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$percent = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'percent',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('19'),
					$elm$svg$Svg$Attributes$y1('5'),
					$elm$svg$Svg$Attributes$x2('5'),
					$elm$svg$Svg$Attributes$y2('19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6.5'),
					$elm$svg$Svg$Attributes$cy('6.5'),
					$elm$svg$Svg$Attributes$r('2.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('17.5'),
					$elm$svg$Svg$Attributes$cy('17.5'),
					$elm$svg$Svg$Attributes$r('2.5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phone = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneCall = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-call',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M15.05 5A5 5 0 0 1 19 8.95M15.05 1A9 9 0 0 1 23 8.94m-1 7.98v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneForwarded = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-forwarded',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('19 1 23 5 19 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('5'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneIncoming = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-incoming',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 2 16 8 22 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneMissed = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-missed',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10.68 13.31a16 16 0 0 0 3.41 2.6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7 2 2 0 0 1 1.72 2v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.42 19.42 0 0 1-3.33-2.67m-2.67-3.34a19.79 19.79 0 0 1-3.07-8.63A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('1'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$phoneOutgoing = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'phone-outgoing',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 7 23 1 17 1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('16'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$pieChart = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'pie-chart',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21.21 15.89A10 10 0 1 1 8 2.83')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 12A10 10 0 0 0 12 2v10z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$play = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'play',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('5 3 19 12 5 21 5 3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$playCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'play-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 8 16 12 10 16 10 8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$plus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'plus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('5'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('19'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$plusCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'plus-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$plusSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'plus-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('16'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$pocket = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'pocket',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 3h16a2 2 0 0 1 2 2v6a10 10 0 0 1-10 10A10 10 0 0 1 2 11V5a2 2 0 0 1 2-2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 10 12 14 16 10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$power = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'power',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M18.36 6.64a9 9 0 1 1-12.73 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$printer = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'printer',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('6 9 6 2 18 2 18 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 18H4a2 2 0 0 1-2-2v-5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2h-2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('6'),
					$elm$svg$Svg$Attributes$y('14'),
					$elm$svg$Svg$Attributes$width('12'),
					$elm$svg$Svg$Attributes$height('8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$radio = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'radio',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16.24 7.76a6 6 0 0 1 0 8.49m-8.48-.01a6 6 0 0 1 0-8.49m11.31-2.82a10 10 0 0 1 0 14.14m-14.14 0a10 10 0 0 1 0-14.14')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$refreshCcw = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'refresh-ccw',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('1 4 1 10 7 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 20 23 14 17 14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.49 9A9 9 0 0 0 5.64 5.64L1 10m22 4l-4.64 4.36A9 9 0 0 1 3.51 15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$refreshCw = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'refresh-cw',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 4 23 10 17 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('1 20 1 14 7 14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$repeat = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'repeat',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 1 21 5 17 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 11V9a4 4 0 0 1 4-4h14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 23 3 19 7 15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 13v2a4 4 0 0 1-4 4H3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$rewind = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'rewind',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 19 2 12 11 5 11 19')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 19 13 12 22 5 22 19')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$rotateCcw = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'rotate-ccw',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('1 4 1 10 7 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3.51 15a9 9 0 1 0 2.13-9.36L1 10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$rotateCw = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'rotate-cw',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 4 23 10 17 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.49 15a9 9 0 1 1-2.12-9.36L23 10')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$rss = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'rss',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 11a9 9 0 0 1 9 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 4a16 16 0 0 1 16 16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('5'),
					$elm$svg$Svg$Attributes$cy('19'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$save = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'save',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 21 17 13 7 13 7 21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7 3 7 8 15 8')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$scissors = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'scissors',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('6'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('18'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('4'),
					$elm$svg$Svg$Attributes$x2('8.12'),
					$elm$svg$Svg$Attributes$y2('15.88')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14.47'),
					$elm$svg$Svg$Attributes$y1('14.48'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8.12'),
					$elm$svg$Svg$Attributes$y1('8.12'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$search = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'search',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('11'),
					$elm$svg$Svg$Attributes$cy('11'),
					$elm$svg$Svg$Attributes$r('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('16.65'),
					$elm$svg$Svg$Attributes$y2('16.65')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$send = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'send',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('22'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('11'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 2 15 22 11 13 2 9 22 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$server = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'server',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('8'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('14'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('8'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('6.01'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('6.01'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$settings = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'settings',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$share = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'share',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4 12v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 6 12 2 8 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$share2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'share-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('5'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('6'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18'),
					$elm$svg$Svg$Attributes$cy('19'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8.59'),
					$elm$svg$Svg$Attributes$y1('13.51'),
					$elm$svg$Svg$Attributes$x2('15.42'),
					$elm$svg$Svg$Attributes$y2('17.49')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15.41'),
					$elm$svg$Svg$Attributes$y1('6.51'),
					$elm$svg$Svg$Attributes$x2('8.59'),
					$elm$svg$Svg$Attributes$y2('10.49')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$shield = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'shield',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$shieldOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'shield-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19.69 14a6.9 6.9 0 0 0 .31-2V5l-8-3-3.16 1.18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M4.73 4.73L4 5v7c0 6 8 10 8 10a20.29 20.29 0 0 0 5.62-4.38')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$shoppingBag = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'shopping-bag',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 2L3 6v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2V6l-3-4z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('3'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 10a4 4 0 0 1-8 0')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$shoppingCart = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'shopping-cart',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('9'),
					$elm$svg$Svg$Attributes$cy('21'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('20'),
					$elm$svg$Svg$Attributes$cy('21'),
					$elm$svg$Svg$Attributes$r('1')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$shuffle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'shuffle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 3 21 3 21 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('21 16 21 21 16 21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('15'),
					$elm$svg$Svg$Attributes$x2('21'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('4'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$sidebar = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'sidebar',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$skipBack = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'skip-back',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('19 20 9 12 19 4 19 20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('5'),
					$elm$svg$Svg$Attributes$y2('5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$skipForward = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'skip-forward',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('5 4 15 12 5 20 5 4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('19'),
					$elm$svg$Svg$Attributes$y1('5'),
					$elm$svg$Svg$Attributes$x2('19'),
					$elm$svg$Svg$Attributes$y2('19')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$slack = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'slack',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14.5 10c-.83 0-1.5-.67-1.5-1.5v-5c0-.83.67-1.5 1.5-1.5s1.5.67 1.5 1.5v5c0 .83-.67 1.5-1.5 1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.5 10H19V8.5c0-.83.67-1.5 1.5-1.5s1.5.67 1.5 1.5-.67 1.5-1.5 1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9.5 14c.83 0 1.5.67 1.5 1.5v5c0 .83-.67 1.5-1.5 1.5S8 21.33 8 20.5v-5c0-.83.67-1.5 1.5-1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3.5 14H5v1.5c0 .83-.67 1.5-1.5 1.5S2 16.33 2 15.5 2.67 14 3.5 14z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 14.5c0-.83.67-1.5 1.5-1.5h5c.83 0 1.5.67 1.5 1.5s-.67 1.5-1.5 1.5h-5c-.83 0-1.5-.67-1.5-1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M15.5 19H14v1.5c0 .83.67 1.5 1.5 1.5s1.5-.67 1.5-1.5-.67-1.5-1.5-1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10 9.5C10 8.67 9.33 8 8.5 8h-5C2.67 8 2 8.67 2 9.5S2.67 11 3.5 11h5c.83 0 1.5-.67 1.5-1.5z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8.5 5H10V3.5C10 2.67 9.33 2 8.5 2S7 2.67 7 3.5 7.67 5 8.5 5z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$slash = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'slash',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.93'),
					$elm$svg$Svg$Attributes$y1('4.93'),
					$elm$svg$Svg$Attributes$x2('19.07'),
					$elm$svg$Svg$Attributes$y2('19.07')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$sliders = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'sliders',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('4'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('10'),
					$elm$svg$Svg$Attributes$x2('4'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('14'),
					$elm$svg$Svg$Attributes$x2('7'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$smartphone = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'smartphone',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('5'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('14'),
					$elm$svg$Svg$Attributes$height('20'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$smile = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'smile',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8 14s1.5 2 4 2 4-2 4-2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15.01'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$speaker = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'speaker',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('4'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('16'),
					$elm$svg$Svg$Attributes$height('20'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('14'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$square = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$star = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'star',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$stopCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'stop-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('9'),
					$elm$svg$Svg$Attributes$y('9'),
					$elm$svg$Svg$Attributes$width('6'),
					$elm$svg$Svg$Attributes$height('6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$sun = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'sun',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.22'),
					$elm$svg$Svg$Attributes$y1('4.22'),
					$elm$svg$Svg$Attributes$x2('5.64'),
					$elm$svg$Svg$Attributes$y2('5.64')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18.36'),
					$elm$svg$Svg$Attributes$y1('18.36'),
					$elm$svg$Svg$Attributes$x2('19.78'),
					$elm$svg$Svg$Attributes$y2('19.78')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('12')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.22'),
					$elm$svg$Svg$Attributes$y1('19.78'),
					$elm$svg$Svg$Attributes$x2('5.64'),
					$elm$svg$Svg$Attributes$y2('18.36')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18.36'),
					$elm$svg$Svg$Attributes$y1('5.64'),
					$elm$svg$Svg$Attributes$x2('19.78'),
					$elm$svg$Svg$Attributes$y2('4.22')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$sunrise = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'sunrise',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 18a5 5 0 0 0-10 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('2'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.22'),
					$elm$svg$Svg$Attributes$y1('10.22'),
					$elm$svg$Svg$Attributes$x2('5.64'),
					$elm$svg$Svg$Attributes$y2('11.64')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18.36'),
					$elm$svg$Svg$Attributes$y1('11.64'),
					$elm$svg$Svg$Attributes$x2('19.78'),
					$elm$svg$Svg$Attributes$y2('10.22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('1'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 6 12 2 16 6')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$sunset = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'sunset',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 18a5 5 0 0 0-10 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4.22'),
					$elm$svg$Svg$Attributes$y1('10.22'),
					$elm$svg$Svg$Attributes$x2('5.64'),
					$elm$svg$Svg$Attributes$y2('11.64')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('3'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18.36'),
					$elm$svg$Svg$Attributes$y1('11.64'),
					$elm$svg$Svg$Attributes$x2('19.78'),
					$elm$svg$Svg$Attributes$y2('10.22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('22'),
					$elm$svg$Svg$Attributes$x2('1'),
					$elm$svg$Svg$Attributes$y2('22')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 5 12 9 8 5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$tablet = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'tablet',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('4'),
					$elm$svg$Svg$Attributes$y('2'),
					$elm$svg$Svg$Attributes$width('16'),
					$elm$svg$Svg$Attributes$height('20'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('18'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$tag = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'tag',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('7'),
					$elm$svg$Svg$Attributes$y1('7'),
					$elm$svg$Svg$Attributes$x2('7.01'),
					$elm$svg$Svg$Attributes$y2('7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$target = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'target',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$terminal = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'terminal',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('4 17 10 11 4 5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('19'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('19')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$thermometer = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'thermometer',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$thumbsDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'thumbs-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10 15v4a3 3 0 0 0 3 3l4-9V2H5.72a2 2 0 0 0-2 1.7l-1.38 9a2 2 0 0 0 2 2.3zm7-13h2.67A2.31 2.31 0 0 1 22 4v7a2.31 2.31 0 0 1-2.33 2H17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$thumbsUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'thumbs-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3zM7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$toggleLeft = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'toggle-left',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('5'),
					$elm$svg$Svg$Attributes$width('22'),
					$elm$svg$Svg$Attributes$height('14'),
					$elm$svg$Svg$Attributes$rx('7'),
					$elm$svg$Svg$Attributes$ry('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$toggleRight = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'toggle-right',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('5'),
					$elm$svg$Svg$Attributes$width('22'),
					$elm$svg$Svg$Attributes$height('14'),
					$elm$svg$Svg$Attributes$rx('7'),
					$elm$svg$Svg$Attributes$ry('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('16'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('3')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$tool = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'tool',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 7.94-7.94l-3.76 3.76z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$trash = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'trash',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3 6 5 6 21 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$trash2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'trash-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('3 6 5 6 21 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('10'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('10'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('14'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$trello = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'trello',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('7'),
					$elm$svg$Svg$Attributes$y('7'),
					$elm$svg$Svg$Attributes$width('3'),
					$elm$svg$Svg$Attributes$height('9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('14'),
					$elm$svg$Svg$Attributes$y('7'),
					$elm$svg$Svg$Attributes$width('3'),
					$elm$svg$Svg$Attributes$height('5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$trendingDown = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'trending-down',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 18 13.5 8.5 8.5 13.5 1 6')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 18 23 18 23 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$trendingUp = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'trending-up',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 6 13.5 15.5 8.5 10.5 1 18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 6 23 6 23 12')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$triangle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'triangle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$truck = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'truck',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('15'),
					$elm$svg$Svg$Attributes$height('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 8 20 8 23 11 23 16 16 16 16 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('5.5'),
					$elm$svg$Svg$Attributes$cy('18.5'),
					$elm$svg$Svg$Attributes$r('2.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18.5'),
					$elm$svg$Svg$Attributes$cy('18.5'),
					$elm$svg$Svg$Attributes$r('2.5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$tv = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'tv',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('2'),
					$elm$svg$Svg$Attributes$y('7'),
					$elm$svg$Svg$Attributes$width('20'),
					$elm$svg$Svg$Attributes$height('15'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 2 12 7 7 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$twitch = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'twitch',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 2H3v16h5v4l4-4h5l4-4V2zm-10 9V7m5 4V7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$twitter = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'twitter',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M23 3a10.9 10.9 0 0 1-3.14 1.53 4.48 4.48 0 0 0-7.86 3v1A10.66 10.66 0 0 1 3 4s-4 9 5 13a11.64 11.64 0 0 1-7 2c9 5 20 0 20-11.5a4.5 4.5 0 0 0-.08-.83A7.72 7.72 0 0 0 23 3z')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$type_ = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'type',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('4 7 4 4 20 4 20 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('4'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$umbrella = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'umbrella',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M23 12a11.05 11.05 0 0 0-22 0zm-5 7a3 3 0 0 1-6 0v-7')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$underline = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'underline',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 3v7a6 6 0 0 0 6 6 6 6 0 0 0 6-6V3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('4'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$unlock = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'unlock',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('11'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('11'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M7 11V7a5 5 0 0 1 9.9-1')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$upload = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'upload',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 8 12 3 7 8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('3'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$uploadCloud = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'upload-cloud',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 16 12 12 8 16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('12'),
					$elm$svg$Svg$Attributes$x2('12'),
					$elm$svg$Svg$Attributes$y2('21')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20.39 18.39A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('16 16 12 12 8 16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$user = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$userCheck = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user-check',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8.5'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('17 11 19 13 23 9')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$userMinus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user-minus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8.5'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$userPlus = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user-plus',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8.5'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('20'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('20'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$userX = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'user-x',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('8.5'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('13')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$users = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'users',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('9'),
					$elm$svg$Svg$Attributes$cy('7'),
					$elm$svg$Svg$Attributes$r('4')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M23 21v-2a4 4 0 0 0-3-3.87')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 3.13a4 4 0 0 1 0 7.75')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$video = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'video',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('23 7 16 12 23 17 23 7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('1'),
					$elm$svg$Svg$Attributes$y('5'),
					$elm$svg$Svg$Attributes$width('15'),
					$elm$svg$Svg$Attributes$height('14'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$videoOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'video-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16 16v1a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V7a2 2 0 0 1 2-2h2m5.66 0H14a2 2 0 0 1 2 2v3.34l1 1L23 7v10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$voicemail = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'voicemail',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('5.5'),
					$elm$svg$Svg$Attributes$cy('11.5'),
					$elm$svg$Svg$Attributes$r('4.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('18.5'),
					$elm$svg$Svg$Attributes$cy('11.5'),
					$elm$svg$Svg$Attributes$r('4.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('5.5'),
					$elm$svg$Svg$Attributes$y1('16'),
					$elm$svg$Svg$Attributes$x2('18.5'),
					$elm$svg$Svg$Attributes$y2('16')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$volume = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'volume',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 5 6 9 2 9 2 15 6 15 11 19 11 5')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$volume1 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'volume-1',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 5 6 9 2 9 2 15 6 15 11 19 11 5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M15.54 8.46a5 5 0 0 1 0 7.07')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$volume2 = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'volume-2',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 5 6 9 2 9 2 15 6 15 11 19 11 5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19.07 4.93a10 10 0 0 1 0 14.14M15.54 8.46a5 5 0 0 1 0 7.07')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$volumeX = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'volume-x',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('11 5 6 9 2 9 2 15 6 15 11 19 11 5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('23'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('17'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('17'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$watch = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'watch',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('7')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12 9 12 12 13.5 13.5')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16.51 17.35l-.35 3.83a2 2 0 0 1-2 1.82H9.83a2 2 0 0 1-2-1.82l-.35-3.83m.01-10.7l.35-3.83A2 2 0 0 1 9.83 1h4.35a2 2 0 0 1 2 1.82l.35 3.83')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$wifi = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'wifi',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 12.55a11 11 0 0 1 14.08 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M1.42 9a16 16 0 0 1 21.16 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8.53 16.11a6 6 0 0 1 6.95 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$wifiOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'wifi-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M16.72 11.06A10.94 10.94 0 0 1 19 12.55')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M5 12.55a10.94 10.94 0 0 1 5.17-2.39')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10.71 5.05A16 16 0 0 1 22.58 9')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M1.42 9a15.91 15.91 0 0 1 4.7-2.88')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M8.53 16.11a6 6 0 0 1 6.95 0')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('20'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('20')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$wind = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'wind',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9.59 4.59A2 2 0 1 1 11 8H2m10.59 11.41A2 2 0 1 0 14 16H2m15.73-8.27A2.5 2.5 0 1 1 19.5 12H2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$x = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'x',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('18'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('6'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('6'),
					$elm$svg$Svg$Attributes$y1('6'),
					$elm$svg$Svg$Attributes$x2('18'),
					$elm$svg$Svg$Attributes$y2('18')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$xCircle = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'x-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$xOctagon = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'x-octagon',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$xSquare = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'x-square',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('3'),
					$elm$svg$Svg$Attributes$y('3'),
					$elm$svg$Svg$Attributes$width('18'),
					$elm$svg$Svg$Attributes$height('18'),
					$elm$svg$Svg$Attributes$rx('2'),
					$elm$svg$Svg$Attributes$ry('2')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$youtube = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'youtube',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22.54 6.42a2.78 2.78 0 0 0-1.94-2C18.88 4 12 4 12 4s-6.88 0-8.6.46a2.78 2.78 0 0 0-1.94 2A29 29 0 0 0 1 11.75a29 29 0 0 0 .46 5.33A2.78 2.78 0 0 0 3.4 19c1.72.46 8.6.46 8.6.46s6.88 0 8.6-.46a2.78 2.78 0 0 0 1.94-2 29 29 0 0 0 .46-5.25 29 29 0 0 0-.46-5.33z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('9.75 15.02 15.5 11.75 9.75 8.48 9.75 15.02')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$zap = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'zap',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('13 2 3 14 12 14 11 22 21 10 12 10 13 2')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$zapOff = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'zap-off',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('12.41 6.75 13 2 10.57 4.92')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('18.57 12.91 21 10 15.66 10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('8 8 3 14 12 14 11 22 16 16')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('1'),
					$elm$svg$Svg$Attributes$y1('1'),
					$elm$svg$Svg$Attributes$x2('23'),
					$elm$svg$Svg$Attributes$y2('23')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$zoomIn = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'zoom-in',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('11'),
					$elm$svg$Svg$Attributes$cy('11'),
					$elm$svg$Svg$Attributes$r('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('16.65'),
					$elm$svg$Svg$Attributes$y2('16.65')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('11'),
					$elm$svg$Svg$Attributes$y1('8'),
					$elm$svg$Svg$Attributes$x2('11'),
					$elm$svg$Svg$Attributes$y2('14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$zoomOut = A2(
	$feathericons$elm_feather$FeatherIcons$makeBuilder,
	'zoom-out',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('11'),
					$elm$svg$Svg$Attributes$cy('11'),
					$elm$svg$Svg$Attributes$r('8')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('21'),
					$elm$svg$Svg$Attributes$y1('21'),
					$elm$svg$Svg$Attributes$x2('16.65'),
					$elm$svg$Svg$Attributes$y2('16.65')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('8'),
					$elm$svg$Svg$Attributes$y1('11'),
					$elm$svg$Svg$Attributes$x2('14'),
					$elm$svg$Svg$Attributes$y2('11')
				]),
			_List_Nil)
		]));
var $feathericons$elm_feather$FeatherIcons$icons = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('activity', $feathericons$elm_feather$FeatherIcons$activity),
			_Utils_Tuple2('airplay', $feathericons$elm_feather$FeatherIcons$airplay),
			_Utils_Tuple2('alert-circle', $feathericons$elm_feather$FeatherIcons$alertCircle),
			_Utils_Tuple2('alert-octagon', $feathericons$elm_feather$FeatherIcons$alertOctagon),
			_Utils_Tuple2('alert-triangle', $feathericons$elm_feather$FeatherIcons$alertTriangle),
			_Utils_Tuple2('align-center', $feathericons$elm_feather$FeatherIcons$alignCenter),
			_Utils_Tuple2('align-justify', $feathericons$elm_feather$FeatherIcons$alignJustify),
			_Utils_Tuple2('align-left', $feathericons$elm_feather$FeatherIcons$alignLeft),
			_Utils_Tuple2('align-right', $feathericons$elm_feather$FeatherIcons$alignRight),
			_Utils_Tuple2('anchor', $feathericons$elm_feather$FeatherIcons$anchor),
			_Utils_Tuple2('aperture', $feathericons$elm_feather$FeatherIcons$aperture),
			_Utils_Tuple2('archive', $feathericons$elm_feather$FeatherIcons$archive),
			_Utils_Tuple2('arrow-down-circle', $feathericons$elm_feather$FeatherIcons$arrowDownCircle),
			_Utils_Tuple2('arrow-down-left', $feathericons$elm_feather$FeatherIcons$arrowDownLeft),
			_Utils_Tuple2('arrow-down-right', $feathericons$elm_feather$FeatherIcons$arrowDownRight),
			_Utils_Tuple2('arrow-down', $feathericons$elm_feather$FeatherIcons$arrowDown),
			_Utils_Tuple2('arrow-left-circle', $feathericons$elm_feather$FeatherIcons$arrowLeftCircle),
			_Utils_Tuple2('arrow-left', $feathericons$elm_feather$FeatherIcons$arrowLeft),
			_Utils_Tuple2('arrow-right-circle', $feathericons$elm_feather$FeatherIcons$arrowRightCircle),
			_Utils_Tuple2('arrow-right', $feathericons$elm_feather$FeatherIcons$arrowRight),
			_Utils_Tuple2('arrow-up-circle', $feathericons$elm_feather$FeatherIcons$arrowUpCircle),
			_Utils_Tuple2('arrow-up-left', $feathericons$elm_feather$FeatherIcons$arrowUpLeft),
			_Utils_Tuple2('arrow-up-right', $feathericons$elm_feather$FeatherIcons$arrowUpRight),
			_Utils_Tuple2('arrow-up', $feathericons$elm_feather$FeatherIcons$arrowUp),
			_Utils_Tuple2('at-sign', $feathericons$elm_feather$FeatherIcons$atSign),
			_Utils_Tuple2('award', $feathericons$elm_feather$FeatherIcons$award),
			_Utils_Tuple2('bar-chart-2', $feathericons$elm_feather$FeatherIcons$barChart2),
			_Utils_Tuple2('bar-chart', $feathericons$elm_feather$FeatherIcons$barChart),
			_Utils_Tuple2('battery-charging', $feathericons$elm_feather$FeatherIcons$batteryCharging),
			_Utils_Tuple2('battery', $feathericons$elm_feather$FeatherIcons$battery),
			_Utils_Tuple2('bell-off', $feathericons$elm_feather$FeatherIcons$bellOff),
			_Utils_Tuple2('bell', $feathericons$elm_feather$FeatherIcons$bell),
			_Utils_Tuple2('bluetooth', $feathericons$elm_feather$FeatherIcons$bluetooth),
			_Utils_Tuple2('bold', $feathericons$elm_feather$FeatherIcons$bold),
			_Utils_Tuple2('book-open', $feathericons$elm_feather$FeatherIcons$bookOpen),
			_Utils_Tuple2('book', $feathericons$elm_feather$FeatherIcons$book),
			_Utils_Tuple2('bookmark', $feathericons$elm_feather$FeatherIcons$bookmark),
			_Utils_Tuple2('box', $feathericons$elm_feather$FeatherIcons$box),
			_Utils_Tuple2('briefcase', $feathericons$elm_feather$FeatherIcons$briefcase),
			_Utils_Tuple2('calendar', $feathericons$elm_feather$FeatherIcons$calendar),
			_Utils_Tuple2('camera-off', $feathericons$elm_feather$FeatherIcons$cameraOff),
			_Utils_Tuple2('camera', $feathericons$elm_feather$FeatherIcons$camera),
			_Utils_Tuple2('cast', $feathericons$elm_feather$FeatherIcons$cast),
			_Utils_Tuple2('check-circle', $feathericons$elm_feather$FeatherIcons$checkCircle),
			_Utils_Tuple2('check-square', $feathericons$elm_feather$FeatherIcons$checkSquare),
			_Utils_Tuple2('check', $feathericons$elm_feather$FeatherIcons$check),
			_Utils_Tuple2('chevron-down', $feathericons$elm_feather$FeatherIcons$chevronDown),
			_Utils_Tuple2('chevron-left', $feathericons$elm_feather$FeatherIcons$chevronLeft),
			_Utils_Tuple2('chevron-right', $feathericons$elm_feather$FeatherIcons$chevronRight),
			_Utils_Tuple2('chevron-up', $feathericons$elm_feather$FeatherIcons$chevronUp),
			_Utils_Tuple2('chevrons-down', $feathericons$elm_feather$FeatherIcons$chevronsDown),
			_Utils_Tuple2('chevrons-left', $feathericons$elm_feather$FeatherIcons$chevronsLeft),
			_Utils_Tuple2('chevrons-right', $feathericons$elm_feather$FeatherIcons$chevronsRight),
			_Utils_Tuple2('chevrons-up', $feathericons$elm_feather$FeatherIcons$chevronsUp),
			_Utils_Tuple2('chrome', $feathericons$elm_feather$FeatherIcons$chrome),
			_Utils_Tuple2('circle', $feathericons$elm_feather$FeatherIcons$circle),
			_Utils_Tuple2('clipboard', $feathericons$elm_feather$FeatherIcons$clipboard),
			_Utils_Tuple2('clock', $feathericons$elm_feather$FeatherIcons$clock),
			_Utils_Tuple2('cloud-drizzle', $feathericons$elm_feather$FeatherIcons$cloudDrizzle),
			_Utils_Tuple2('cloud-lightning', $feathericons$elm_feather$FeatherIcons$cloudLightning),
			_Utils_Tuple2('cloud-off', $feathericons$elm_feather$FeatherIcons$cloudOff),
			_Utils_Tuple2('cloud-rain', $feathericons$elm_feather$FeatherIcons$cloudRain),
			_Utils_Tuple2('cloud-snow', $feathericons$elm_feather$FeatherIcons$cloudSnow),
			_Utils_Tuple2('cloud', $feathericons$elm_feather$FeatherIcons$cloud),
			_Utils_Tuple2('code', $feathericons$elm_feather$FeatherIcons$code),
			_Utils_Tuple2('codepen', $feathericons$elm_feather$FeatherIcons$codepen),
			_Utils_Tuple2('codesandbox', $feathericons$elm_feather$FeatherIcons$codesandbox),
			_Utils_Tuple2('coffee', $feathericons$elm_feather$FeatherIcons$coffee),
			_Utils_Tuple2('columns', $feathericons$elm_feather$FeatherIcons$columns),
			_Utils_Tuple2('command', $feathericons$elm_feather$FeatherIcons$command),
			_Utils_Tuple2('compass', $feathericons$elm_feather$FeatherIcons$compass),
			_Utils_Tuple2('copy', $feathericons$elm_feather$FeatherIcons$copy),
			_Utils_Tuple2('corner-down-left', $feathericons$elm_feather$FeatherIcons$cornerDownLeft),
			_Utils_Tuple2('corner-down-right', $feathericons$elm_feather$FeatherIcons$cornerDownRight),
			_Utils_Tuple2('corner-left-down', $feathericons$elm_feather$FeatherIcons$cornerLeftDown),
			_Utils_Tuple2('corner-left-up', $feathericons$elm_feather$FeatherIcons$cornerLeftUp),
			_Utils_Tuple2('corner-right-down', $feathericons$elm_feather$FeatherIcons$cornerRightDown),
			_Utils_Tuple2('corner-right-up', $feathericons$elm_feather$FeatherIcons$cornerRightUp),
			_Utils_Tuple2('corner-up-left', $feathericons$elm_feather$FeatherIcons$cornerUpLeft),
			_Utils_Tuple2('corner-up-right', $feathericons$elm_feather$FeatherIcons$cornerUpRight),
			_Utils_Tuple2('cpu', $feathericons$elm_feather$FeatherIcons$cpu),
			_Utils_Tuple2('credit-card', $feathericons$elm_feather$FeatherIcons$creditCard),
			_Utils_Tuple2('crop', $feathericons$elm_feather$FeatherIcons$crop),
			_Utils_Tuple2('crosshair', $feathericons$elm_feather$FeatherIcons$crosshair),
			_Utils_Tuple2('database', $feathericons$elm_feather$FeatherIcons$database),
			_Utils_Tuple2('delete', $feathericons$elm_feather$FeatherIcons$delete),
			_Utils_Tuple2('disc', $feathericons$elm_feather$FeatherIcons$disc),
			_Utils_Tuple2('divide-circle', $feathericons$elm_feather$FeatherIcons$divideCircle),
			_Utils_Tuple2('divide-square', $feathericons$elm_feather$FeatherIcons$divideSquare),
			_Utils_Tuple2('divide', $feathericons$elm_feather$FeatherIcons$divide),
			_Utils_Tuple2('dollar-sign', $feathericons$elm_feather$FeatherIcons$dollarSign),
			_Utils_Tuple2('download-cloud', $feathericons$elm_feather$FeatherIcons$downloadCloud),
			_Utils_Tuple2('download', $feathericons$elm_feather$FeatherIcons$download),
			_Utils_Tuple2('dribbble', $feathericons$elm_feather$FeatherIcons$dribbble),
			_Utils_Tuple2('droplet', $feathericons$elm_feather$FeatherIcons$droplet),
			_Utils_Tuple2('edit-2', $feathericons$elm_feather$FeatherIcons$edit2),
			_Utils_Tuple2('edit-3', $feathericons$elm_feather$FeatherIcons$edit3),
			_Utils_Tuple2('edit', $feathericons$elm_feather$FeatherIcons$edit),
			_Utils_Tuple2('external-link', $feathericons$elm_feather$FeatherIcons$externalLink),
			_Utils_Tuple2('eye-off', $feathericons$elm_feather$FeatherIcons$eyeOff),
			_Utils_Tuple2('eye', $feathericons$elm_feather$FeatherIcons$eye),
			_Utils_Tuple2('facebook', $feathericons$elm_feather$FeatherIcons$facebook),
			_Utils_Tuple2('fast-forward', $feathericons$elm_feather$FeatherIcons$fastForward),
			_Utils_Tuple2('feather', $feathericons$elm_feather$FeatherIcons$feather),
			_Utils_Tuple2('figma', $feathericons$elm_feather$FeatherIcons$figma),
			_Utils_Tuple2('file-minus', $feathericons$elm_feather$FeatherIcons$fileMinus),
			_Utils_Tuple2('file-plus', $feathericons$elm_feather$FeatherIcons$filePlus),
			_Utils_Tuple2('file-text', $feathericons$elm_feather$FeatherIcons$fileText),
			_Utils_Tuple2('file', $feathericons$elm_feather$FeatherIcons$file),
			_Utils_Tuple2('film', $feathericons$elm_feather$FeatherIcons$film),
			_Utils_Tuple2('filter', $feathericons$elm_feather$FeatherIcons$filter),
			_Utils_Tuple2('flag', $feathericons$elm_feather$FeatherIcons$flag),
			_Utils_Tuple2('folder-minus', $feathericons$elm_feather$FeatherIcons$folderMinus),
			_Utils_Tuple2('folder-plus', $feathericons$elm_feather$FeatherIcons$folderPlus),
			_Utils_Tuple2('folder', $feathericons$elm_feather$FeatherIcons$folder),
			_Utils_Tuple2('framer', $feathericons$elm_feather$FeatherIcons$framer),
			_Utils_Tuple2('frown', $feathericons$elm_feather$FeatherIcons$frown),
			_Utils_Tuple2('gift', $feathericons$elm_feather$FeatherIcons$gift),
			_Utils_Tuple2('git-branch', $feathericons$elm_feather$FeatherIcons$gitBranch),
			_Utils_Tuple2('git-commit', $feathericons$elm_feather$FeatherIcons$gitCommit),
			_Utils_Tuple2('git-merge', $feathericons$elm_feather$FeatherIcons$gitMerge),
			_Utils_Tuple2('git-pull-request', $feathericons$elm_feather$FeatherIcons$gitPullRequest),
			_Utils_Tuple2('github', $feathericons$elm_feather$FeatherIcons$github),
			_Utils_Tuple2('gitlab', $feathericons$elm_feather$FeatherIcons$gitlab),
			_Utils_Tuple2('globe', $feathericons$elm_feather$FeatherIcons$globe),
			_Utils_Tuple2('grid', $feathericons$elm_feather$FeatherIcons$grid),
			_Utils_Tuple2('hard-drive', $feathericons$elm_feather$FeatherIcons$hardDrive),
			_Utils_Tuple2('hash', $feathericons$elm_feather$FeatherIcons$hash),
			_Utils_Tuple2('headphones', $feathericons$elm_feather$FeatherIcons$headphones),
			_Utils_Tuple2('heart', $feathericons$elm_feather$FeatherIcons$heart),
			_Utils_Tuple2('help-circle', $feathericons$elm_feather$FeatherIcons$helpCircle),
			_Utils_Tuple2('hexagon', $feathericons$elm_feather$FeatherIcons$hexagon),
			_Utils_Tuple2('home', $feathericons$elm_feather$FeatherIcons$home),
			_Utils_Tuple2('image', $feathericons$elm_feather$FeatherIcons$image),
			_Utils_Tuple2('inbox', $feathericons$elm_feather$FeatherIcons$inbox),
			_Utils_Tuple2('info', $feathericons$elm_feather$FeatherIcons$info),
			_Utils_Tuple2('instagram', $feathericons$elm_feather$FeatherIcons$instagram),
			_Utils_Tuple2('italic', $feathericons$elm_feather$FeatherIcons$italic),
			_Utils_Tuple2('key', $feathericons$elm_feather$FeatherIcons$key),
			_Utils_Tuple2('layers', $feathericons$elm_feather$FeatherIcons$layers),
			_Utils_Tuple2('layout', $feathericons$elm_feather$FeatherIcons$layout),
			_Utils_Tuple2('life-buoy', $feathericons$elm_feather$FeatherIcons$lifeBuoy),
			_Utils_Tuple2('link-2', $feathericons$elm_feather$FeatherIcons$link2),
			_Utils_Tuple2('link', $feathericons$elm_feather$FeatherIcons$link),
			_Utils_Tuple2('linkedin', $feathericons$elm_feather$FeatherIcons$linkedin),
			_Utils_Tuple2('list', $feathericons$elm_feather$FeatherIcons$list),
			_Utils_Tuple2('loader', $feathericons$elm_feather$FeatherIcons$loader),
			_Utils_Tuple2('lock', $feathericons$elm_feather$FeatherIcons$lock),
			_Utils_Tuple2('log-in', $feathericons$elm_feather$FeatherIcons$logIn),
			_Utils_Tuple2('log-out', $feathericons$elm_feather$FeatherIcons$logOut),
			_Utils_Tuple2('mail', $feathericons$elm_feather$FeatherIcons$mail),
			_Utils_Tuple2('map-pin', $feathericons$elm_feather$FeatherIcons$mapPin),
			_Utils_Tuple2('map', $feathericons$elm_feather$FeatherIcons$map),
			_Utils_Tuple2('maximize-2', $feathericons$elm_feather$FeatherIcons$maximize2),
			_Utils_Tuple2('maximize', $feathericons$elm_feather$FeatherIcons$maximize),
			_Utils_Tuple2('meh', $feathericons$elm_feather$FeatherIcons$meh),
			_Utils_Tuple2('menu', $feathericons$elm_feather$FeatherIcons$menu),
			_Utils_Tuple2('message-circle', $feathericons$elm_feather$FeatherIcons$messageCircle),
			_Utils_Tuple2('message-square', $feathericons$elm_feather$FeatherIcons$messageSquare),
			_Utils_Tuple2('mic-off', $feathericons$elm_feather$FeatherIcons$micOff),
			_Utils_Tuple2('mic', $feathericons$elm_feather$FeatherIcons$mic),
			_Utils_Tuple2('minimize-2', $feathericons$elm_feather$FeatherIcons$minimize2),
			_Utils_Tuple2('minimize', $feathericons$elm_feather$FeatherIcons$minimize),
			_Utils_Tuple2('minus-circle', $feathericons$elm_feather$FeatherIcons$minusCircle),
			_Utils_Tuple2('minus-square', $feathericons$elm_feather$FeatherIcons$minusSquare),
			_Utils_Tuple2('minus', $feathericons$elm_feather$FeatherIcons$minus),
			_Utils_Tuple2('monitor', $feathericons$elm_feather$FeatherIcons$monitor),
			_Utils_Tuple2('moon', $feathericons$elm_feather$FeatherIcons$moon),
			_Utils_Tuple2('more-horizontal', $feathericons$elm_feather$FeatherIcons$moreHorizontal),
			_Utils_Tuple2('more-vertical', $feathericons$elm_feather$FeatherIcons$moreVertical),
			_Utils_Tuple2('mouse-pointer', $feathericons$elm_feather$FeatherIcons$mousePointer),
			_Utils_Tuple2('move', $feathericons$elm_feather$FeatherIcons$move),
			_Utils_Tuple2('music', $feathericons$elm_feather$FeatherIcons$music),
			_Utils_Tuple2('navigation-2', $feathericons$elm_feather$FeatherIcons$navigation2),
			_Utils_Tuple2('navigation', $feathericons$elm_feather$FeatherIcons$navigation),
			_Utils_Tuple2('octagon', $feathericons$elm_feather$FeatherIcons$octagon),
			_Utils_Tuple2('package', $feathericons$elm_feather$FeatherIcons$package),
			_Utils_Tuple2('paperclip', $feathericons$elm_feather$FeatherIcons$paperclip),
			_Utils_Tuple2('pause-circle', $feathericons$elm_feather$FeatherIcons$pauseCircle),
			_Utils_Tuple2('pause', $feathericons$elm_feather$FeatherIcons$pause),
			_Utils_Tuple2('pen-tool', $feathericons$elm_feather$FeatherIcons$penTool),
			_Utils_Tuple2('percent', $feathericons$elm_feather$FeatherIcons$percent),
			_Utils_Tuple2('phone-call', $feathericons$elm_feather$FeatherIcons$phoneCall),
			_Utils_Tuple2('phone-forwarded', $feathericons$elm_feather$FeatherIcons$phoneForwarded),
			_Utils_Tuple2('phone-incoming', $feathericons$elm_feather$FeatherIcons$phoneIncoming),
			_Utils_Tuple2('phone-missed', $feathericons$elm_feather$FeatherIcons$phoneMissed),
			_Utils_Tuple2('phone-off', $feathericons$elm_feather$FeatherIcons$phoneOff),
			_Utils_Tuple2('phone-outgoing', $feathericons$elm_feather$FeatherIcons$phoneOutgoing),
			_Utils_Tuple2('phone', $feathericons$elm_feather$FeatherIcons$phone),
			_Utils_Tuple2('pie-chart', $feathericons$elm_feather$FeatherIcons$pieChart),
			_Utils_Tuple2('play-circle', $feathericons$elm_feather$FeatherIcons$playCircle),
			_Utils_Tuple2('play', $feathericons$elm_feather$FeatherIcons$play),
			_Utils_Tuple2('plus-circle', $feathericons$elm_feather$FeatherIcons$plusCircle),
			_Utils_Tuple2('plus-square', $feathericons$elm_feather$FeatherIcons$plusSquare),
			_Utils_Tuple2('plus', $feathericons$elm_feather$FeatherIcons$plus),
			_Utils_Tuple2('pocket', $feathericons$elm_feather$FeatherIcons$pocket),
			_Utils_Tuple2('power', $feathericons$elm_feather$FeatherIcons$power),
			_Utils_Tuple2('printer', $feathericons$elm_feather$FeatherIcons$printer),
			_Utils_Tuple2('radio', $feathericons$elm_feather$FeatherIcons$radio),
			_Utils_Tuple2('refresh-ccw', $feathericons$elm_feather$FeatherIcons$refreshCcw),
			_Utils_Tuple2('refresh-cw', $feathericons$elm_feather$FeatherIcons$refreshCw),
			_Utils_Tuple2('repeat', $feathericons$elm_feather$FeatherIcons$repeat),
			_Utils_Tuple2('rewind', $feathericons$elm_feather$FeatherIcons$rewind),
			_Utils_Tuple2('rotate-ccw', $feathericons$elm_feather$FeatherIcons$rotateCcw),
			_Utils_Tuple2('rotate-cw', $feathericons$elm_feather$FeatherIcons$rotateCw),
			_Utils_Tuple2('rss', $feathericons$elm_feather$FeatherIcons$rss),
			_Utils_Tuple2('save', $feathericons$elm_feather$FeatherIcons$save),
			_Utils_Tuple2('scissors', $feathericons$elm_feather$FeatherIcons$scissors),
			_Utils_Tuple2('search', $feathericons$elm_feather$FeatherIcons$search),
			_Utils_Tuple2('send', $feathericons$elm_feather$FeatherIcons$send),
			_Utils_Tuple2('server', $feathericons$elm_feather$FeatherIcons$server),
			_Utils_Tuple2('settings', $feathericons$elm_feather$FeatherIcons$settings),
			_Utils_Tuple2('share-2', $feathericons$elm_feather$FeatherIcons$share2),
			_Utils_Tuple2('share', $feathericons$elm_feather$FeatherIcons$share),
			_Utils_Tuple2('shield-off', $feathericons$elm_feather$FeatherIcons$shieldOff),
			_Utils_Tuple2('shield', $feathericons$elm_feather$FeatherIcons$shield),
			_Utils_Tuple2('shopping-bag', $feathericons$elm_feather$FeatherIcons$shoppingBag),
			_Utils_Tuple2('shopping-cart', $feathericons$elm_feather$FeatherIcons$shoppingCart),
			_Utils_Tuple2('shuffle', $feathericons$elm_feather$FeatherIcons$shuffle),
			_Utils_Tuple2('sidebar', $feathericons$elm_feather$FeatherIcons$sidebar),
			_Utils_Tuple2('skip-back', $feathericons$elm_feather$FeatherIcons$skipBack),
			_Utils_Tuple2('skip-forward', $feathericons$elm_feather$FeatherIcons$skipForward),
			_Utils_Tuple2('slack', $feathericons$elm_feather$FeatherIcons$slack),
			_Utils_Tuple2('slash', $feathericons$elm_feather$FeatherIcons$slash),
			_Utils_Tuple2('sliders', $feathericons$elm_feather$FeatherIcons$sliders),
			_Utils_Tuple2('smartphone', $feathericons$elm_feather$FeatherIcons$smartphone),
			_Utils_Tuple2('smile', $feathericons$elm_feather$FeatherIcons$smile),
			_Utils_Tuple2('speaker', $feathericons$elm_feather$FeatherIcons$speaker),
			_Utils_Tuple2('square', $feathericons$elm_feather$FeatherIcons$square),
			_Utils_Tuple2('star', $feathericons$elm_feather$FeatherIcons$star),
			_Utils_Tuple2('stop-circle', $feathericons$elm_feather$FeatherIcons$stopCircle),
			_Utils_Tuple2('sun', $feathericons$elm_feather$FeatherIcons$sun),
			_Utils_Tuple2('sunrise', $feathericons$elm_feather$FeatherIcons$sunrise),
			_Utils_Tuple2('sunset', $feathericons$elm_feather$FeatherIcons$sunset),
			_Utils_Tuple2('tablet', $feathericons$elm_feather$FeatherIcons$tablet),
			_Utils_Tuple2('tag', $feathericons$elm_feather$FeatherIcons$tag),
			_Utils_Tuple2('target', $feathericons$elm_feather$FeatherIcons$target),
			_Utils_Tuple2('terminal', $feathericons$elm_feather$FeatherIcons$terminal),
			_Utils_Tuple2('thermometer', $feathericons$elm_feather$FeatherIcons$thermometer),
			_Utils_Tuple2('thumbs-down', $feathericons$elm_feather$FeatherIcons$thumbsDown),
			_Utils_Tuple2('thumbs-up', $feathericons$elm_feather$FeatherIcons$thumbsUp),
			_Utils_Tuple2('toggle-left', $feathericons$elm_feather$FeatherIcons$toggleLeft),
			_Utils_Tuple2('toggle-right', $feathericons$elm_feather$FeatherIcons$toggleRight),
			_Utils_Tuple2('tool', $feathericons$elm_feather$FeatherIcons$tool),
			_Utils_Tuple2('trash-2', $feathericons$elm_feather$FeatherIcons$trash2),
			_Utils_Tuple2('trash', $feathericons$elm_feather$FeatherIcons$trash),
			_Utils_Tuple2('trello', $feathericons$elm_feather$FeatherIcons$trello),
			_Utils_Tuple2('trending-down', $feathericons$elm_feather$FeatherIcons$trendingDown),
			_Utils_Tuple2('trending-up', $feathericons$elm_feather$FeatherIcons$trendingUp),
			_Utils_Tuple2('triangle', $feathericons$elm_feather$FeatherIcons$triangle),
			_Utils_Tuple2('truck', $feathericons$elm_feather$FeatherIcons$truck),
			_Utils_Tuple2('tv', $feathericons$elm_feather$FeatherIcons$tv),
			_Utils_Tuple2('twitch', $feathericons$elm_feather$FeatherIcons$twitch),
			_Utils_Tuple2('twitter', $feathericons$elm_feather$FeatherIcons$twitter),
			_Utils_Tuple2('type', $feathericons$elm_feather$FeatherIcons$type_),
			_Utils_Tuple2('umbrella', $feathericons$elm_feather$FeatherIcons$umbrella),
			_Utils_Tuple2('underline', $feathericons$elm_feather$FeatherIcons$underline),
			_Utils_Tuple2('unlock', $feathericons$elm_feather$FeatherIcons$unlock),
			_Utils_Tuple2('upload-cloud', $feathericons$elm_feather$FeatherIcons$uploadCloud),
			_Utils_Tuple2('upload', $feathericons$elm_feather$FeatherIcons$upload),
			_Utils_Tuple2('user-check', $feathericons$elm_feather$FeatherIcons$userCheck),
			_Utils_Tuple2('user-minus', $feathericons$elm_feather$FeatherIcons$userMinus),
			_Utils_Tuple2('user-plus', $feathericons$elm_feather$FeatherIcons$userPlus),
			_Utils_Tuple2('user-x', $feathericons$elm_feather$FeatherIcons$userX),
			_Utils_Tuple2('user', $feathericons$elm_feather$FeatherIcons$user),
			_Utils_Tuple2('users', $feathericons$elm_feather$FeatherIcons$users),
			_Utils_Tuple2('video-off', $feathericons$elm_feather$FeatherIcons$videoOff),
			_Utils_Tuple2('video', $feathericons$elm_feather$FeatherIcons$video),
			_Utils_Tuple2('voicemail', $feathericons$elm_feather$FeatherIcons$voicemail),
			_Utils_Tuple2('volume-1', $feathericons$elm_feather$FeatherIcons$volume1),
			_Utils_Tuple2('volume-2', $feathericons$elm_feather$FeatherIcons$volume2),
			_Utils_Tuple2('volume-x', $feathericons$elm_feather$FeatherIcons$volumeX),
			_Utils_Tuple2('volume', $feathericons$elm_feather$FeatherIcons$volume),
			_Utils_Tuple2('watch', $feathericons$elm_feather$FeatherIcons$watch),
			_Utils_Tuple2('wifi-off', $feathericons$elm_feather$FeatherIcons$wifiOff),
			_Utils_Tuple2('wifi', $feathericons$elm_feather$FeatherIcons$wifi),
			_Utils_Tuple2('wind', $feathericons$elm_feather$FeatherIcons$wind),
			_Utils_Tuple2('x-circle', $feathericons$elm_feather$FeatherIcons$xCircle),
			_Utils_Tuple2('x-octagon', $feathericons$elm_feather$FeatherIcons$xOctagon),
			_Utils_Tuple2('x-square', $feathericons$elm_feather$FeatherIcons$xSquare),
			_Utils_Tuple2('x', $feathericons$elm_feather$FeatherIcons$x),
			_Utils_Tuple2('youtube', $feathericons$elm_feather$FeatherIcons$youtube),
			_Utils_Tuple2('zap-off', $feathericons$elm_feather$FeatherIcons$zapOff),
			_Utils_Tuple2('zap', $feathericons$elm_feather$FeatherIcons$zap),
			_Utils_Tuple2('zoom-in', $feathericons$elm_feather$FeatherIcons$zoomIn),
			_Utils_Tuple2('zoom-out', $feathericons$elm_feather$FeatherIcons$zoomOut)
		]));
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $author$project$Utils$stopPropagationOnMousedown = function (msg_) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'mousedown',
		$elm$json$Json$Decode$succeed(
			_Utils_Tuple2(msg_, true)));
};
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $author$project$IconMenuAPI$viewIconList = A2(
	$elm$core$List$map,
	function (_v0) {
		var iconName = _v0.a;
		var icon = _v0.b;
		return A2(
			$elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$AppModel$IconMenu(
							$author$project$IconMenu$SetIcon(
								$elm$core$Maybe$Just(iconName)))),
						$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp),
						$elm$html$Html$Attributes$title(iconName)
					]),
				$author$project$IconMenuAPI$iconButtonStyle),
			_List_fromArray(
				[
					A2($feathericons$elm_feather$FeatherIcons$toHtml, _List_Nil, icon)
				]));
	},
	$elm$core$Dict$toList($feathericons$elm_feather$FeatherIcons$icons));
var $feathericons$elm_feather$FeatherIcons$withSize = F2(
	function (size, _v0) {
		var attrs = _v0.a.attrs;
		var src = _v0.a.src;
		return $feathericons$elm_feather$FeatherIcons$Icon(
			{
				attrs: _Utils_update(
					attrs,
					{size: size}),
				src: src
			});
	});
var $author$project$IconMenuAPI$viewIconMenu = function (model) {
	return model.iconMenu.open ? _List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			$author$project$IconMenuAPI$iconMenuStyle,
			_List_fromArray(
				[
					A2($elm$html$Html$div, $author$project$IconMenuAPI$iconListStyle, $author$project$IconMenuAPI$viewIconList),
					A2(
					$elm$html$Html$button,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$AppModel$IconMenu($author$project$IconMenu$Close))
							]),
						$author$project$IconMenuAPI$closeButtonStyle),
					_List_fromArray(
						[
							A2(
							$feathericons$elm_feather$FeatherIcons$toHtml,
							_List_Nil,
							A2($feathericons$elm_feather$FeatherIcons$withSize, 12, $feathericons$elm_feather$FeatherIcons$x))
						]))
				]))
		]) : _List_Nil;
};
var $author$project$MapRenderer$blackBoxStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'pointer-events', 'none')
	]);
var $author$project$Config$blackBoxOffset = 5;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$MapRenderer$selectionStyle = F3(
	function (topicId, mapId, model) {
		var selected = A2(
			$elm$core$List$member,
			_Utils_Tuple2(topicId, mapId),
			model.selection);
		return selected ? _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'box-shadow', 'gray 5px 5px 5px')
			]) : _List_Nil;
	});
var $author$project$MapRenderer$topicBorderStyle = F3(
	function (id, mapId, model) {
		var targeted = function () {
			var _v0 = model.mouse.dragState;
			_v0$2:
			while (true) {
				if (_v0.$ === 'Drag') {
					if (_v0.a.$ === 'DragTopic') {
						if (_v0.f.$ === 'Just') {
							var _v1 = _v0.a;
							var mapId_ = _v0.c;
							var target = _v0.f.a;
							return _Utils_eq(
								target,
								_Utils_Tuple2(id, mapId)) && (!_Utils_eq(mapId_, id));
						} else {
							break _v0$2;
						}
					} else {
						if (_v0.f.$ === 'Just') {
							var _v2 = _v0.a;
							var mapId_ = _v0.c;
							var target = _v0.f.a;
							return _Utils_eq(
								target,
								_Utils_Tuple2(id, mapId)) && _Utils_eq(mapId_, mapId);
						} else {
							break _v0$2;
						}
					}
				} else {
					break _v0$2;
				}
			}
			return false;
		}();
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$style,
				'border-width',
				$elm$core$String$fromFloat($author$project$Config$topicBorderWidth) + 'px'),
				A2(
				$elm$html$Html$Attributes$style,
				'border-style',
				targeted ? 'dashed' : 'solid'),
				A2($elm$html$Html$Attributes$style, 'box-sizing', 'border-box'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'white')
			]);
	});
var $author$project$Config$topicRadius = 7;
var $author$project$MapRenderer$ghostTopicStyle = F3(
	function (topic, mapId, model) {
		return _Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromInt($author$project$Config$blackBoxOffset) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromInt($author$project$Config$blackBoxOffset) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat($author$project$Config$topicSize.w) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'height',
					$elm$core$String$fromFloat($author$project$Config$topicSize.h) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'border-radius',
					$elm$core$String$fromInt($author$project$Config$topicRadius) + 'px'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
					A2($elm$html$Html$Attributes$style, 'z-index', '-1')
				]),
			_Utils_ap(
				A3($author$project$MapRenderer$topicBorderStyle, topic.id, mapId, model),
				A3($author$project$MapRenderer$selectionStyle, topic.id, mapId, model)));
	});
var $author$project$Model$EditEnd = {$: 'EditEnd'};
var $author$project$Model$OnTextInput = function (a) {
	return {$: 'OnTextInput', a: a};
};
var $elm$core$String$lines = _String_lines;
var $author$project$ModelAPI$getTopicLabel = function (topic) {
	var _v0 = $elm$core$List$head(
		$elm$core$String$lines(topic.text));
	if (_v0.$ === 'Just') {
		var line = _v0.a;
		return line;
	} else {
		return '';
	}
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$keyCode = A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int);
var $author$project$Utils$keyDecoder = F2(
	function (key, msg_) {
		var isKey = function (code) {
			return _Utils_eq(code, key) ? $elm$json$Json$Decode$succeed(msg_) : $elm$json$Json$Decode$fail('not that key');
		};
		return A2($elm$json$Json$Decode$andThen, isKey, $elm$html$Html$Events$keyCode);
	});
var $author$project$Utils$onEnterOrEsc = function (msg_) {
	return A2(
		$elm$html$Html$Events$on,
		'keydown',
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($author$project$Utils$keyDecoder, 13, msg_),
					A2($author$project$Utils$keyDecoder, 27, msg_)
				])));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $author$project$MapRenderer$topicIconBoxStyle = function (props) {
	var r1 = $elm$core$String$fromInt($author$project$Config$topicRadius) + 'px';
	var r4 = function () {
		var _v0 = props.displayMode;
		if ((_v0.$ === 'Container') && (_v0.a.$ === 'WhiteBox')) {
			var _v1 = _v0.a;
			return '0';
		} else {
			return r1;
		}
	}();
	return _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'flex', 'none'),
			A2(
			$elm$html$Html$Attributes$style,
			'width',
			$elm$core$String$fromFloat($author$project$Config$topicSize.h) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'height',
			$elm$core$String$fromFloat($author$project$Config$topicSize.h) + 'px'),
			A2($elm$html$Html$Attributes$style, 'border-radius', r1 + (' 0 0 ' + r4)),
			A2($elm$html$Html$Attributes$style, 'background-color', 'black'),
			A2($elm$html$Html$Attributes$style, 'pointer-events', 'none')
		]);
};
var $author$project$Config$topicLabelWeight = 'bold';
var $author$project$MapRenderer$topicInputStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'font-family', $author$project$Config$mainFont),
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'font-weight', $author$project$Config$topicLabelWeight),
		A2($elm$html$Html$Attributes$style, 'width', '100%'),
		A2($elm$html$Html$Attributes$style, 'position', 'relative'),
		A2($elm$html$Html$Attributes$style, 'left', '-4px'),
		A2($elm$html$Html$Attributes$style, 'pointer-events', 'initial')
	]);
var $author$project$MapRenderer$topicLabelStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'font-weight', $author$project$Config$topicLabelWeight),
		A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
		A2($elm$html$Html$Attributes$style, 'text-overflow', 'ellipsis'),
		A2($elm$html$Html$Attributes$style, 'white-space', 'nowrap'),
		A2($elm$html$Html$Attributes$style, 'pointer-events', 'none')
	]);
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$ModelAPI$getTopicInfo = F2(
	function (topicId, model) {
		var _v0 = A2($elm$core$Dict$get, topicId, model.items);
		if (_v0.$ === 'Just') {
			var item = _v0.a;
			if (item.$ === 'Topic') {
				var topic = item.a;
				return $elm$core$Maybe$Just(topic);
			} else {
				return A3($author$project$ModelAPI$topicMismatch, 'getTopicInfo', topicId, $elm$core$Maybe$Nothing);
			}
		} else {
			return A3($author$project$ModelAPI$illegalItemId, 'getTopicInfo', topicId, $elm$core$Maybe$Nothing);
		}
	});
var $author$project$Config$topicIconSize = 16;
var $author$project$IconMenuAPI$topicIconStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'relative'),
		A2(
		$elm$html$Html$Attributes$style,
		'top',
		$elm$core$String$fromFloat(($author$project$Config$topicSize.h - $author$project$Config$topicIconSize) / 2) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'left',
		$elm$core$String$fromFloat(($author$project$Config$topicSize.h - $author$project$Config$topicIconSize) / 2) + 'px'),
		A2($elm$html$Html$Attributes$style, 'color', 'white')
	]);
var $author$project$IconMenuAPI$viewTopicIcon = F2(
	function (topicId, model) {
		var _v0 = A2($author$project$ModelAPI$getTopicInfo, topicId, model);
		if (_v0.$ === 'Just') {
			var topic = _v0.a;
			var _v1 = topic.iconName;
			if (_v1.$ === 'Just') {
				var iconName = _v1.a;
				var _v2 = A2($elm$core$Dict$get, iconName, $feathericons$elm_feather$FeatherIcons$icons);
				if (_v2.$ === 'Just') {
					var icon = _v2.a;
					return A2(
						$feathericons$elm_feather$FeatherIcons$toHtml,
						$author$project$IconMenuAPI$topicIconStyle,
						A2($feathericons$elm_feather$FeatherIcons$withSize, $author$project$Config$topicIconSize, icon));
				} else {
					return $elm$html$Html$text('??');
				}
			} else {
				return $elm$html$Html$text('');
			}
		} else {
			return $elm$html$Html$text('?');
		}
	});
var $author$project$MapRenderer$labelTopicHtml = F4(
	function (topic, props, mapId, model) {
		var isEdit = _Utils_eq(
			model.editState,
			A2($author$project$Model$ItemEdit, topic.id, mapId));
		var textElem = isEdit ? A2(
			$elm$html$Html$input,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id(
						'dmx-input-' + ($elm$core$String$fromInt(topic.id) + ('-' + $elm$core$String$fromInt(mapId)))),
						$elm$html$Html$Attributes$value(topic.text),
						$elm$html$Html$Events$onInput(
						A2($elm$core$Basics$composeL, $author$project$AppModel$Edit, $author$project$Model$OnTextInput)),
						$elm$html$Html$Events$onBlur(
						$author$project$AppModel$Edit($author$project$Model$EditEnd)),
						$author$project$Utils$onEnterOrEsc(
						$author$project$AppModel$Edit($author$project$Model$EditEnd)),
						$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp)
					]),
				$author$project$MapRenderer$topicInputStyle),
			_List_Nil) : A2(
			$elm$html$Html$div,
			$author$project$MapRenderer$topicLabelStyle,
			_List_fromArray(
				[
					$elm$html$Html$text(
					$author$project$ModelAPI$getTopicLabel(topic))
				]));
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				$author$project$MapRenderer$topicIconBoxStyle(props),
				_List_fromArray(
					[
						A2($author$project$IconMenuAPI$viewTopicIcon, topic.id, model)
					])),
				textElem
			]);
	});
var $author$project$MapRenderer$itemCountStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'left', 'calc(100% + 12px)')
	]);
var $author$project$MapRenderer$mapItemCount = F3(
	function (topicId, props, model) {
		var itemCount = function () {
			var _v0 = props.displayMode;
			if (_v0.$ === 'Monad') {
				return 0;
			} else {
				var _v1 = A2($author$project$ModelAPI$getMap, topicId, model.maps);
				if (_v1.$ === 'Just') {
					var map = _v1.a;
					return $elm$core$List$length(
						A2(
							$elm$core$List$filter,
							$author$project$ModelAPI$isVisible,
							$elm$core$Dict$values(map.items)));
				} else {
					return 0;
				}
			}
		}();
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				$author$project$MapRenderer$itemCountStyle,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(itemCount))
					]))
			]);
	});
var $author$project$MapRenderer$topicFlexboxStyle = F4(
	function (topic, props, mapId, model) {
		var r12 = $elm$core$String$fromInt($author$project$Config$topicRadius) + 'px';
		var r34 = function () {
			var _v0 = props.displayMode;
			if ((_v0.$ === 'Container') && (_v0.a.$ === 'WhiteBox')) {
				var _v1 = _v0.a;
				return '0';
			} else {
				return r12;
			}
		}();
		return _Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'gap', '8px'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat($author$project$Config$topicSize.w) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'height',
					$elm$core$String$fromFloat($author$project$Config$topicSize.h) + 'px'),
					A2($elm$html$Html$Attributes$style, 'border-radius', r12 + (' ' + (r12 + (' ' + (r34 + (' ' + r34))))))
				]),
			A3($author$project$MapRenderer$topicBorderStyle, topic.id, mapId, model));
	});
var $author$project$MapRenderer$topicPosStyle = function (_v0) {
	var pos = _v0.pos;
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$Attributes$style,
			'left',
			$elm$core$String$fromFloat(pos.x - $author$project$Config$topicW2) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'top',
			$elm$core$String$fromFloat(pos.y - $author$project$Config$topicH2) + 'px')
		]);
};
var $author$project$MapRenderer$blackBoxTopic = F4(
	function (topic, props, mapId, model) {
		return _Utils_Tuple2(
			$author$project$MapRenderer$topicPosStyle(props),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_Utils_ap(
						A4($author$project$MapRenderer$topicFlexboxStyle, topic, props, mapId, model),
						$author$project$MapRenderer$blackBoxStyle),
					_Utils_ap(
						A4($author$project$MapRenderer$labelTopicHtml, topic, props, mapId, model),
						A3($author$project$MapRenderer$mapItemCount, topic.id, props, model))),
					A2(
					$elm$html$Html$div,
					A3($author$project$MapRenderer$ghostTopicStyle, topic, mapId, model),
					_List_Nil)
				]));
	});
var $author$project$Model$OnTextareaInput = function (a) {
	return {$: 'OnTextareaInput', a: a};
};
var $author$project$ModelAPI$getTopicSize = F3(
	function (topicId, mapId, maps) {
		var _v0 = A3($author$project$ModelAPI$getTopicProps, topicId, mapId, maps);
		if (_v0.$ === 'Just') {
			var size = _v0.a.size;
			return $elm$core$Maybe$Just(size);
		} else {
			return A3(
				$author$project$Utils$fail,
				'getTopicSize',
				{mapId: mapId, topicId: topicId},
				$elm$core$Maybe$Nothing);
		}
	});
var $author$project$MapRenderer$detailTextEditStyle = F3(
	function (topicId, mapId, model) {
		var height = function () {
			var _v0 = A3($author$project$ModelAPI$getTopicSize, topicId, mapId, model.maps);
			if (_v0.$ === 'Just') {
				var size = _v0.a;
				return size.h;
			} else {
				return 0;
			}
		}();
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'relative'),
				A2(
				$elm$html$Html$Attributes$style,
				'top',
				$elm$core$String$fromFloat(-$author$project$Config$topicBorderWidth) + 'px'),
				A2(
				$elm$html$Html$Attributes$style,
				'height',
				$elm$core$String$fromFloat(height) + 'px'),
				A2($elm$html$Html$Attributes$style, 'font-family', $author$project$Config$mainFont),
				A2($elm$html$Html$Attributes$style, 'border-color', 'black'),
				A2($elm$html$Html$Attributes$style, 'resize', 'none')
			]);
	});
var $author$project$MapRenderer$detailTextStyle = F3(
	function (topicId, mapId, model) {
		var r = $elm$core$String$fromInt($author$project$Config$topicRadius) + 'px';
		return _Utils_ap(
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$style,
					'font-size',
					$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat($author$project$Config$topicDetailMaxWidth) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'line-height',
					$elm$core$String$fromFloat($author$project$Config$topicLineHeight)),
					A2(
					$elm$html$Html$Attributes$style,
					'padding',
					$elm$core$String$fromInt($author$project$Config$topicDetailPadding) + 'px'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '0 ' + (r + (' ' + (r + (' ' + r)))))
				]),
			_Utils_ap(
				A3($author$project$MapRenderer$topicBorderStyle, topicId, mapId, model),
				A3($author$project$MapRenderer$selectionStyle, topicId, mapId, model)));
	});
var $author$project$MapRenderer$detailTextViewStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'min-width',
		$elm$core$String$fromFloat($author$project$Config$topicSize.w - $author$project$Config$topicSize.h) + 'px'),
		A2($elm$html$Html$Attributes$style, 'max-width', 'max-content'),
		A2($elm$html$Html$Attributes$style, 'white-space', 'pre-wrap'),
		A2($elm$html$Html$Attributes$style, 'pointer-events', 'none')
	]);
var $author$project$MapRenderer$detailTopicIconBoxStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'padding-left',
		$elm$core$String$fromFloat($author$project$Config$topicBorderWidth) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromFloat($author$project$Config$topicSize.h - $author$project$Config$topicBorderWidth) + 'px')
	]);
var $author$project$MapRenderer$detailTopicStyle = function (_v0) {
	var pos = _v0.pos;
	return _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'display', 'flex'),
			A2(
			$elm$html$Html$Attributes$style,
			'left',
			$elm$core$String$fromFloat(pos.x - $author$project$Config$topicW2) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'top',
			$elm$core$String$fromFloat(pos.y - $author$project$Config$topicH2) + 'px')
		]);
};
var $author$project$Utils$multilineHtml = function (str) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (line, linesAcc) {
				return _Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$text(line),
							A2($elm$html$Html$br, _List_Nil, _List_Nil)
						]),
					linesAcc);
			}),
		_List_Nil,
		$elm$core$String$lines(str));
};
var $author$project$Utils$onEsc = function (msg_) {
	return A2(
		$elm$html$Html$Events$on,
		'keydown',
		A2($author$project$Utils$keyDecoder, 27, msg_));
};
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$MapRenderer$detailTopic = F4(
	function (topic, props, mapId, model) {
		var isEdit = _Utils_eq(
			model.editState,
			A2($author$project$Model$ItemEdit, topic.id, mapId));
		var textElem = isEdit ? A2(
			$elm$html$Html$textarea,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id(
						'dmx-input-' + ($elm$core$String$fromInt(topic.id) + ('-' + $elm$core$String$fromInt(mapId)))),
						$elm$html$Html$Events$onInput(
						A2($elm$core$Basics$composeL, $author$project$AppModel$Edit, $author$project$Model$OnTextareaInput)),
						$elm$html$Html$Events$onBlur(
						$author$project$AppModel$Edit($author$project$Model$EditEnd)),
						$author$project$Utils$onEsc(
						$author$project$AppModel$Edit($author$project$Model$EditEnd)),
						$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp)
					]),
				_Utils_ap(
					A3($author$project$MapRenderer$detailTextStyle, topic.id, mapId, model),
					A3($author$project$MapRenderer$detailTextEditStyle, topic.id, mapId, model))),
			_List_fromArray(
				[
					$elm$html$Html$text(topic.text)
				])) : A2(
			$elm$html$Html$div,
			_Utils_ap(
				A3($author$project$MapRenderer$detailTextStyle, topic.id, mapId, model),
				$author$project$MapRenderer$detailTextViewStyle),
			$author$project$Utils$multilineHtml(topic.text));
		return _Utils_Tuple2(
			$author$project$MapRenderer$detailTopicStyle(props),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_Utils_ap(
						$author$project$MapRenderer$topicIconBoxStyle(props),
						_Utils_ap(
							$author$project$MapRenderer$detailTopicIconBoxStyle,
							A3($author$project$MapRenderer$selectionStyle, topic.id, mapId, model))),
					_List_fromArray(
						[
							A2($author$project$IconMenuAPI$viewTopicIcon, topic.id, model)
						])),
					textElem
				]));
	});
var $author$project$MapRenderer$effectiveDisplayMode = F3(
	function (topicId, displayMode, model) {
		var isLimbo = _Utils_eq(
			model.search.menu,
			$author$project$Search$Open(
				$elm$core$Maybe$Just(topicId)));
		if (isLimbo) {
			if (displayMode.$ === 'Monad') {
				return $author$project$Model$Monad($author$project$Model$Detail);
			} else {
				return $author$project$Model$Container($author$project$Model$WhiteBox);
			}
		} else {
			return displayMode;
		}
	});
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $author$project$MapRenderer$gAttr = F3(
	function (mapId, mapRect, model) {
		return _List_fromArray(
			[
				$elm$svg$Svg$Attributes$transform(
				'translate(' + ($elm$core$String$fromFloat(-mapRect.x1) + (' ' + ($elm$core$String$fromFloat(-mapRect.y1) + ')'))))
			]);
	});
var $author$project$ModelAPI$isFullscreen = F2(
	function (mapId, model) {
		return _Utils_eq(
			$author$project$ModelAPI$activeMap(model),
			mapId);
	});
var $author$project$MapRenderer$labelTopic = F4(
	function (topic, props, mapId, model) {
		return _Utils_Tuple2(
			_Utils_ap(
				$author$project$MapRenderer$topicPosStyle(props),
				_Utils_ap(
					A4($author$project$MapRenderer$topicFlexboxStyle, topic, props, mapId, model),
					A3($author$project$MapRenderer$selectionStyle, topic.id, mapId, model))),
			A4($author$project$MapRenderer$labelTopicHtml, topic, props, mapId, model));
	});
var $elm$core$Basics$round = _Basics_round;
var $author$project$MapRenderer$svgStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '0'),
		A2($elm$html$Html$Attributes$style, 'left', '0')
	]);
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $author$project$MapRenderer$topicAttr = F3(
	function (topicId, mapId, model) {
		return A2($author$project$ModelAPI$isFullscreen, topicId, model) ? _List_Nil : _List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'class', 'dmx-topic'),
				A2(
				$elm$html$Html$Attributes$attribute,
				'data-id',
				$elm$core$String$fromInt(topicId)),
				A2(
				$elm$html$Html$Attributes$attribute,
				'data-map-id',
				$elm$core$String$fromInt(mapId))
			]);
	});
var $author$project$MapRenderer$topicLayerStyle = function (mapRect) {
	return _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
			A2(
			$elm$html$Html$Attributes$style,
			'left',
			$elm$core$String$fromFloat(-mapRect.x1) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'top',
			$elm$core$String$fromFloat(-mapRect.y1) + 'px')
		]);
};
var $author$project$MapRenderer$topicStyle = F3(
	function (_v0, mapId, model) {
		var id = _v0.id;
		var isLimbo = _Utils_eq(
			model.search.menu,
			$author$project$Search$Open(
				$elm$core$Maybe$Just(id)));
		var isDragging = function () {
			var _v1 = model.mouse.dragState;
			if ((_v1.$ === 'Drag') && (_v1.a.$ === 'DragTopic')) {
				var _v2 = _v1.a;
				var id_ = _v1.b;
				return _Utils_eq(id_, id);
			} else {
				return false;
			}
		}();
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
				A2(
				$elm$html$Html$Attributes$style,
				'opacity',
				isLimbo ? '.5' : '1'),
				A2(
				$elm$html$Html$Attributes$style,
				'z-index',
				isDragging ? '1' : '2')
			]);
	});
var $author$project$MapRenderer$unboxedTopic = F4(
	function (topic, props, mapId, model) {
		var _v0 = A4($author$project$MapRenderer$labelTopic, topic, props, mapId, model);
		var style = _v0.a;
		var children = _v0.b;
		return _Utils_Tuple2(
			style,
			_Utils_ap(
				children,
				A3($author$project$MapRenderer$mapItemCount, topic.id, props, model)));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$MapRenderer$assocGeometry = F3(
	function (assoc, mapId, model) {
		var pos2 = A3($author$project$ModelAPI$getTopicPos, assoc.player2, mapId, model.maps);
		var pos1 = A3($author$project$ModelAPI$getTopicPos, assoc.player1, mapId, model.maps);
		var _v0 = A3(
			$elm$core$Maybe$map2,
			F2(
				function (p1, p2) {
					return _Utils_Tuple2(p1, p2);
				}),
			pos1,
			pos2);
		if (_v0.$ === 'Just') {
			var geometry = _v0.a;
			return $elm$core$Maybe$Just(geometry);
		} else {
			return A3(
				$author$project$Utils$fail,
				'assocGeometry',
				{assoc: assoc, mapId: mapId},
				$elm$core$Maybe$Nothing);
		}
	});
var $author$project$Config$assocRadius = 14;
var $author$project$Config$assocColor = 'black';
var $author$project$Config$assocWith = 1.5;
var $author$project$MapRenderer$lineDasharray = function (maybeAssoc) {
	if (maybeAssoc.$ === 'Just') {
		var itemType = maybeAssoc.a.itemType;
		switch (itemType) {
			case 'dmx.association':
				return '5 0';
			case 'dmx.composition':
				return '5';
			default:
				return '1';
		}
	} else {
		return '5 0';
	}
};
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $author$project$MapRenderer$lineStyle = function (assoc) {
	return _List_fromArray(
		[
			$elm$svg$Svg$Attributes$stroke($author$project$Config$assocColor),
			$elm$svg$Svg$Attributes$strokeWidth(
			$elm$core$String$fromFloat($author$project$Config$assocWith) + 'px'),
			$elm$svg$Svg$Attributes$strokeDasharray(
			$author$project$MapRenderer$lineDasharray(assoc)),
			$elm$svg$Svg$Attributes$fill('none')
		]);
};
var $author$project$MapRenderer$taxiLine = F3(
	function (assoc, pos1, pos2) {
		if (_Utils_cmp(
			$elm$core$Basics$abs(pos2.x - pos1.x),
			2 * $author$project$Config$assocRadius) < 0) {
			var xm = (pos1.x + pos2.x) / 2;
			return A2(
				$elm$svg$Svg$path,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(xm) + (' ' + ($elm$core$String$fromFloat(pos1.y) + (' V ' + $elm$core$String$fromFloat(pos2.y))))))
						]),
					$author$project$MapRenderer$lineStyle(assoc)),
				_List_Nil);
		} else {
			if (_Utils_cmp(
				$elm$core$Basics$abs(pos2.y - pos1.y),
				2 * $author$project$Config$assocRadius) < 0) {
				var ym = (pos1.y + pos2.y) / 2;
				return A2(
					$elm$svg$Svg$path,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$d(
								'M ' + ($elm$core$String$fromFloat(pos1.x) + (' ' + ($elm$core$String$fromFloat(ym) + (' H ' + $elm$core$String$fromFloat(pos2.x))))))
							]),
						$author$project$MapRenderer$lineStyle(assoc)),
					_List_Nil);
			} else {
				var ym = (pos1.y + pos2.y) / 2;
				var sy = (_Utils_cmp(pos2.y, pos1.y) > 0) ? (-1) : 1;
				var y1 = $elm$core$String$fromFloat(ym + (sy * $author$project$Config$assocRadius));
				var y2 = $elm$core$String$fromFloat(ym - (sy * $author$project$Config$assocRadius));
				var sx = (_Utils_cmp(pos2.x, pos1.x) > 0) ? 1 : (-1);
				var x1 = $elm$core$String$fromFloat(pos1.x + (sx * $author$project$Config$assocRadius));
				var x2 = $elm$core$String$fromFloat(pos2.x - (sx * $author$project$Config$assocRadius));
				var sweep1 = (sy === 1) ? ((sx === 1) ? 1 : 0) : ((sx === 1) ? 0 : 1);
				var sweep2 = 1 - sweep1;
				var sw2 = $elm$core$String$fromInt(sweep2);
				var sw1 = $elm$core$String$fromInt(sweep1);
				var r = $elm$core$String$fromFloat($author$project$Config$assocRadius);
				return A2(
					$elm$svg$Svg$path,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$d(
								'M ' + ($elm$core$String$fromFloat(pos1.x) + (' ' + ($elm$core$String$fromFloat(pos1.y) + (' V ' + (y1 + (' A ' + (r + (' ' + (r + (' 0 0 ' + (sw1 + (' ' + (x1 + (' ' + ($elm$core$String$fromFloat(ym) + (' H ' + (x2 + (' A ' + (r + (' ' + (r + (' 0 0 ' + (sw2 + (' ' + ($elm$core$String$fromFloat(pos2.x) + (' ' + (y2 + (' V ' + $elm$core$String$fromFloat(pos2.y))))))))))))))))))))))))))))))
							]),
						$author$project$MapRenderer$lineStyle(assoc)),
					_List_Nil);
			}
		}
	});
var $author$project$MapRenderer$lineFunc = $author$project$MapRenderer$taxiLine;
var $author$project$MapRenderer$viewAssoc = F3(
	function (assoc, mapId, model) {
		var geom = A3($author$project$MapRenderer$assocGeometry, assoc, mapId, model);
		if (geom.$ === 'Just') {
			var _v1 = geom.a;
			var pos1 = _v1.a;
			var pos2 = _v1.b;
			return A3(
				$author$project$MapRenderer$lineFunc,
				$elm$core$Maybe$Just(assoc),
				pos1,
				pos2);
		} else {
			return $elm$html$Html$text('');
		}
	});
var $author$project$MapRenderer$absMapPos = F3(
	function (mapId, posAcc, model) {
		return A2(
			$elm$core$Maybe$andThen,
			function (map) {
				var posAcc_ = A2($author$project$Model$Point, posAcc.x - map.rect.x1, posAcc.y - map.rect.y1);
				return A2($author$project$ModelAPI$isFullscreen, mapId, model) ? $elm$core$Maybe$Just(posAcc_) : A2(
					$elm$core$Maybe$andThen,
					function (mapPos) {
						return A3(
							$author$project$MapRenderer$absMapPos,
							map.parentMapId,
							A2($author$project$Model$Point, (posAcc_.x + mapPos.x) - $author$project$Config$topicW2, (posAcc_.y + mapPos.y) + $author$project$Config$topicH2),
							model);
					},
					A3($author$project$ModelAPI$getTopicPos, map.id, map.parentMapId, model.maps));
			},
			A2($author$project$ModelAPI$getMap, mapId, model.maps));
	});
var $author$project$MapRenderer$relPos = F3(
	function (pos, mapId, model) {
		return A2(
			$elm$core$Maybe$andThen,
			function (posAbs) {
				return $elm$core$Maybe$Just(
					A2($author$project$Model$Point, pos.x - posAbs.x, pos.y - posAbs.y));
			},
			A3(
				$author$project$MapRenderer$absMapPos,
				mapId,
				A2($author$project$Model$Point, 0, 0),
				model));
	});
var $author$project$MapRenderer$viewLimboAssoc = F2(
	function (mapId, model) {
		var _v0 = model.mouse.dragState;
		if ((_v0.$ === 'Drag') && (_v0.a.$ === 'DrawAssoc')) {
			var _v1 = _v0.a;
			var topicId = _v0.b;
			var mapId_ = _v0.c;
			var pos = _v0.e;
			if (_Utils_eq(mapId_, mapId)) {
				var points = A3(
					$elm$core$Maybe$map2,
					F2(
						function (pos1, pos2) {
							return _Utils_Tuple2(pos1, pos2);
						}),
					A3($author$project$ModelAPI$getTopicPos, topicId, mapId, model.maps),
					A3($author$project$MapRenderer$relPos, pos, mapId, model));
				if (points.$ === 'Just') {
					var _v3 = points.a;
					var pos1 = _v3.a;
					var pos2 = _v3.b;
					return _List_fromArray(
						[
							A3($author$project$MapRenderer$lineFunc, $elm$core$Maybe$Nothing, pos1, pos2)
						]);
				} else {
					return _List_Nil;
				}
			} else {
				return _List_Nil;
			}
		} else {
			return _List_Nil;
		}
	});
var $author$project$Config$whiteBoxRadius = 14;
var $author$project$MapRenderer$whiteBoxStyle = F4(
	function (topicId, rect, mapId, model) {
		var width = rect.x2 - rect.x1;
		var r = $elm$core$String$fromInt($author$project$Config$whiteBoxRadius) + 'px';
		var height = rect.y2 - rect.y1;
		return _Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromFloat(-$author$project$Config$topicBorderWidth) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromFloat($author$project$Config$topicSize.h - (2 * $author$project$Config$topicBorderWidth)) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat(width) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'height',
					$elm$core$String$fromFloat(height) + 'px'),
					A2($elm$html$Html$Attributes$style, 'border-radius', '0 ' + (r + (' ' + (r + (' ' + r)))))
				]),
			_Utils_ap(
				A3($author$project$MapRenderer$topicBorderStyle, topicId, mapId, model),
				A3($author$project$MapRenderer$selectionStyle, topicId, mapId, model)));
	});
var $author$project$MapRenderer$limboTopic = F2(
	function (mapId, model) {
		var activeMapId = $author$project$ModelAPI$activeMap(model);
		if (_Utils_eq(mapId, activeMapId)) {
			var _v15 = model.search.menu;
			if ((_v15.$ === 'Open') && (_v15.a.$ === 'Just')) {
				var topicId = _v15.a.a;
				if (A3($author$project$ModelAPI$isItemInMap, topicId, activeMapId, model)) {
					var _v16 = A3($author$project$ModelAPI$getMapItemById, topicId, activeMapId, model.maps);
					if (_v16.$ === 'Just') {
						var mapItem = _v16.a;
						if (mapItem.hidden) {
							var _v17 = A2(
								$author$project$Utils$info,
								'limboTopic',
								_Utils_Tuple2(topicId, 'is in map, hidden'));
							var _v18 = _Utils_Tuple2(
								A2($elm$core$Dict$get, topicId, model.items),
								mapItem.props);
							if (((_v18.a.$ === 'Just') && (_v18.a.a.$ === 'Topic')) && (_v18.b.$ === 'MapTopic')) {
								var topic = _v18.a.a.a;
								var props = _v18.b.a;
								return _List_fromArray(
									[
										A4($author$project$MapRenderer$viewTopic, topic, props, activeMapId, model)
									]);
							} else {
								return _List_Nil;
							}
						} else {
							var _v19 = A2(
								$author$project$Utils$info,
								'limboTopic',
								_Utils_Tuple2(topicId, 'is in map, already visible'));
							return _List_Nil;
						}
					} else {
						return _List_Nil;
					}
				} else {
					var props = A3($author$project$ModelAPI$defaultProps, topicId, $author$project$Config$topicSize, model);
					var _v20 = A2(
						$author$project$Utils$info,
						'limboTopic',
						_Utils_Tuple2(topicId, 'not in map'));
					var _v21 = A2($elm$core$Dict$get, topicId, model.items);
					if ((_v21.$ === 'Just') && (_v21.a.$ === 'Topic')) {
						var topic = _v21.a.a;
						return _List_fromArray(
							[
								A4($author$project$MapRenderer$viewTopic, topic, props, activeMapId, model)
							]);
					} else {
						return _List_Nil;
					}
				}
			} else {
				return _List_Nil;
			}
		} else {
			return _List_Nil;
		}
	});
var $author$project$MapRenderer$mapInfo = F3(
	function (mapId, parentMapId, model) {
		var _v14 = A2($author$project$ModelAPI$getMap, mapId, model.maps);
		if (_v14.$ === 'Just') {
			var map = _v14.a;
			return _Utils_Tuple3(
				A2($author$project$MapRenderer$mapItems, map, model),
				map.rect,
				A2($author$project$ModelAPI$isFullscreen, mapId, model) ? _Utils_Tuple2(
					{h: '100%', w: '100%'},
					_List_Nil) : _Utils_Tuple2(
					{
						h: $elm$core$String$fromInt(
							$elm$core$Basics$round(map.rect.y2 - map.rect.y1)),
						w: $elm$core$String$fromInt(
							$elm$core$Basics$round(map.rect.x2 - map.rect.x1))
					},
					A4($author$project$MapRenderer$whiteBoxStyle, mapId, map.rect, parentMapId, model)));
		} else {
			return _Utils_Tuple3(
				_Utils_Tuple2(_List_Nil, _List_Nil),
				A4($author$project$Model$Rectangle, 0, 0, 0, 0),
				_Utils_Tuple2(
					{h: '0', w: '0'},
					_List_Nil));
		}
	});
var $author$project$MapRenderer$mapItems = F2(
	function (map, model) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (_v11, _v12) {
					var id = _v11.id;
					var props = _v11.props;
					var t = _v12.a;
					var a = _v12.b;
					var item = A2($elm$core$Dict$get, id, model.items);
					var _v13 = _Utils_Tuple2(item, props);
					_v13$2:
					while (true) {
						if (_v13.a.$ === 'Just') {
							if (_v13.a.a.$ === 'Topic') {
								if (_v13.b.$ === 'MapTopic') {
									var topic = _v13.a.a.a;
									var tProps = _v13.b.a;
									return _Utils_Tuple2(
										A2(
											$elm$core$List$cons,
											A4($author$project$MapRenderer$viewTopic, topic, tProps, map.id, model),
											t),
										a);
								} else {
									break _v13$2;
								}
							} else {
								if (_v13.b.$ === 'MapAssoc') {
									var assoc = _v13.a.a.a;
									return _Utils_Tuple2(
										t,
										A2(
											$elm$core$List$cons,
											A3($author$project$MapRenderer$viewAssoc, assoc, map.id, model),
											a));
								} else {
									break _v13$2;
								}
							}
						} else {
							break _v13$2;
						}
					}
					return A3(
						$author$project$Utils$logError,
						'mapItems',
						'problem with item ' + $elm$core$String$fromInt(id),
						_Utils_Tuple2(t, a));
				}),
			_Utils_Tuple2(_List_Nil, _List_Nil),
			A2(
				$elm$core$List$filter,
				$author$project$ModelAPI$isVisible,
				$elm$core$Dict$values(map.items)));
	});
var $author$project$MapRenderer$viewMap = F3(
	function (mapId, parentMapId, model) {
		var _v8 = A3($author$project$MapRenderer$mapInfo, mapId, parentMapId, model);
		var _v9 = _v8.a;
		var topics = _v9.a;
		var assocs = _v9.b;
		var mapRect = _v8.b;
		var _v10 = _v8.c;
		var svgSize = _v10.a;
		var mapStyle = _v10.b;
		return A2(
			$elm$html$Html$div,
			mapStyle,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					$author$project$MapRenderer$topicLayerStyle(mapRect),
					_Utils_ap(
						topics,
						A2($author$project$MapRenderer$limboTopic, mapId, model))),
					A2(
					$elm$svg$Svg$svg,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$width(svgSize.w),
								$elm$svg$Svg$Attributes$height(svgSize.h)
							]),
						_Utils_ap(
							A3($author$project$MapRenderer$topicAttr, mapId, parentMapId, model),
							$author$project$MapRenderer$svgStyle)),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$g,
							A3($author$project$MapRenderer$gAttr, mapId, mapRect, model),
							_Utils_ap(
								assocs,
								A2($author$project$MapRenderer$viewLimboAssoc, mapId, model)))
						]))
				]));
	});
var $author$project$MapRenderer$viewTopic = F4(
	function (topic, props, mapId, model) {
		var topicFunc = function () {
			var _v2 = A3($author$project$MapRenderer$effectiveDisplayMode, topic.id, props.displayMode, model);
			if (_v2.$ === 'Monad') {
				if (_v2.a.$ === 'LabelOnly') {
					var _v3 = _v2.a;
					return $author$project$MapRenderer$labelTopic;
				} else {
					var _v4 = _v2.a;
					return $author$project$MapRenderer$detailTopic;
				}
			} else {
				switch (_v2.a.$) {
					case 'BlackBox':
						var _v5 = _v2.a;
						return $author$project$MapRenderer$blackBoxTopic;
					case 'WhiteBox':
						var _v6 = _v2.a;
						return $author$project$MapRenderer$whiteBoxTopic;
					default:
						var _v7 = _v2.a;
						return $author$project$MapRenderer$unboxedTopic;
				}
			}
		}();
		var _v1 = A4(topicFunc, topic, props, mapId, model);
		var style = _v1.a;
		var children = _v1.b;
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				A3($author$project$MapRenderer$topicAttr, topic.id, mapId, model),
				_Utils_ap(
					A3($author$project$MapRenderer$topicStyle, topic, mapId, model),
					style)),
			children);
	});
var $author$project$MapRenderer$whiteBoxTopic = F4(
	function (topic, props, mapId, model) {
		var _v0 = A4($author$project$MapRenderer$labelTopic, topic, props, mapId, model);
		var style = _v0.a;
		var children = _v0.b;
		return _Utils_Tuple2(
			style,
			_Utils_ap(
				children,
				_Utils_ap(
					A3($author$project$MapRenderer$mapItemCount, topic.id, props, model),
					_List_fromArray(
						[
							A3($author$project$MapRenderer$viewMap, topic.id, mapId, model)
						]))));
	});
var $author$project$Search$ClickItem = function (a) {
	return {$: 'ClickItem', a: a};
};
var $author$project$Search$HoverItem = function (a) {
	return {$: 'HoverItem', a: a};
};
var $author$project$Search$UnhoverItem = function (a) {
	return {$: 'UnhoverItem', a: a};
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$AppModel$Search = function (a) {
	return {$: 'Search', a: a};
};
var $author$project$SearchAPI$itemDecoder = function (msg) {
	return A2(
		$elm$json$Json$Decode$map,
		$author$project$AppModel$Search,
		A2(
			$elm$json$Json$Decode$map,
			msg,
			A2(
				$elm$json$Json$Decode$andThen,
				$author$project$Utils$strToIntDecoder,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['target', 'dataset', 'id']),
					$elm$json$Json$Decode$string))));
};
var $author$project$SearchAPI$resultItemStyle = F2(
	function (topicId, model) {
		var isHover = function () {
			var _v0 = model.search.menu;
			if (_v0.$ === 'Open') {
				var maybeId = _v0.a;
				return _Utils_eq(
					maybeId,
					$elm$core$Maybe$Just(topicId));
			} else {
				return false;
			}
		}();
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$style,
				'color',
				isHover ? 'white' : 'black'),
				A2(
				$elm$html$Html$Attributes$style,
				'background-color',
				isHover ? 'black' : 'white'),
				A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
				A2($elm$html$Html$Attributes$style, 'text-overflow', 'ellipsis'),
				A2($elm$html$Html$Attributes$style, 'padding', '0 8px')
			]);
	});
var $author$project$SearchAPI$resultMenuStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '144px'),
		A2($elm$html$Html$Attributes$style, 'width', '240px'),
		A2($elm$html$Html$Attributes$style, 'padding', '3px 0'),
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$contentFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'line-height', '2'),
		A2($elm$html$Html$Attributes$style, 'white-space', 'nowrap'),
		A2($elm$html$Html$Attributes$style, 'background-color', 'white'),
		A2($elm$html$Html$Attributes$style, 'border', '1px solid lightgray'),
		A2($elm$html$Html$Attributes$style, 'z-index', '2')
	]);
var $author$project$SearchAPI$viewResultMenu = function (model) {
	var _v0 = _Utils_Tuple2(
		model.search.menu,
		$elm$core$List$isEmpty(model.search.result));
	if ((_v0.a.$ === 'Open') && (!_v0.b)) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Events$on,
							'click',
							$author$project$SearchAPI$itemDecoder($author$project$Search$ClickItem)),
							A2(
							$elm$html$Html$Events$on,
							'mouseover',
							$author$project$SearchAPI$itemDecoder($author$project$Search$HoverItem)),
							A2(
							$elm$html$Html$Events$on,
							'mouseout',
							$author$project$SearchAPI$itemDecoder($author$project$Search$UnhoverItem)),
							$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp)
						]),
					$author$project$SearchAPI$resultMenuStyle),
				A2(
					$elm$core$List$map,
					function (id) {
						var _v1 = A2($author$project$ModelAPI$getTopicInfo, id, model);
						if (_v1.$ === 'Just') {
							var topic = _v1.a;
							return A2(
								$elm$html$Html$div,
								_Utils_ap(
									_List_fromArray(
										[
											A2(
											$elm$html$Html$Attributes$attribute,
											'data-id',
											$elm$core$String$fromInt(topic.id))
										]),
									A2($author$project$SearchAPI$resultItemStyle, topic.id, model)),
								_List_fromArray(
									[
										$elm$html$Html$text(topic.text)
									]));
						} else {
							return $elm$html$Html$text('??');
						}
					},
					model.search.result))
			]);
	} else {
		return _List_Nil;
	}
};
var $author$project$AppModel$AddTopic = {$: 'AddTopic'};
var $author$project$AppModel$Delete = {$: 'Delete'};
var $author$project$Model$EditStart = {$: 'EditStart'};
var $author$project$Model$Fullscreen = {$: 'Fullscreen'};
var $author$project$AppModel$Hide = {$: 'Hide'};
var $author$project$AppModel$Nav = function (a) {
	return {$: 'Nav', a: a};
};
var $author$project$IconMenu$Open = {$: 'Open'};
var $author$project$Config$toolbarFontSize = 14;
var $author$project$Toolbar$toolbarStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$toolbarFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'display', 'flex'),
		A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
		A2($elm$html$Html$Attributes$style, 'align-items', 'flex-start'),
		A2($elm$html$Html$Attributes$style, 'gap', '28px'),
		A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
		A2($elm$html$Html$Attributes$style, 'z-index', '1')
	]);
var $author$project$AppModel$SwitchDisplay = function (a) {
	return {$: 'SwitchDisplay', a: a};
};
var $author$project$Toolbar$displayModeStyle = function (disabled) {
	var _v0 = disabled ? _Utils_Tuple2('gray', 'none') : _Utils_Tuple2('unset', 'unset');
	var color = _v0.a;
	var pointerEvents = _v0.b;
	return _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'display', 'flex'),
			A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
			A2($elm$html$Html$Attributes$style, 'gap', '6px'),
			A2($elm$html$Html$Attributes$style, 'color', color),
			A2($elm$html$Html$Attributes$style, 'pointer-events', pointerEvents)
		]);
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Toolbar$viewRadioButton = F4(
	function (label_, msg, isChecked, isDisabled) {
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('radio'),
							$elm$html$Html$Attributes$name('display-mode'),
							$elm$html$Html$Attributes$checked(isChecked),
							$elm$html$Html$Attributes$disabled(isDisabled),
							$elm$html$Html$Events$onClick(msg)
						]),
					_List_Nil),
					$elm$html$Html$text(label_)
				]));
	});
var $author$project$Toolbar$viewContainerDisplay = function (model) {
	var displayMode = function () {
		var _v6 = $author$project$ModelAPI$getSingleSelection(model);
		if (_v6.$ === 'Just') {
			var _v7 = _v6.a;
			var topicId = _v7.a;
			var mapId = _v7.b;
			return A3($author$project$ModelAPI$getDisplayMode, topicId, mapId, model.maps);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}();
	var disabled_ = function () {
		if ((displayMode.$ === 'Just') && (displayMode.a.$ === 'Container')) {
			return false;
		} else {
			return true;
		}
	}();
	var _v0 = function () {
		if ((displayMode.$ === 'Just') && (displayMode.a.$ === 'Container')) {
			switch (displayMode.a.a.$) {
				case 'BlackBox':
					var _v2 = displayMode.a.a;
					return _Utils_Tuple3(true, false, false);
				case 'WhiteBox':
					var _v3 = displayMode.a.a;
					return _Utils_Tuple3(false, true, false);
				default:
					var _v4 = displayMode.a.a;
					return _Utils_Tuple3(false, false, true);
			}
		} else {
			return _Utils_Tuple3(false, false, false);
		}
	}();
	var checked1 = _v0.a;
	var checked2 = _v0.b;
	var checked3 = _v0.c;
	return A2(
		$elm$html$Html$div,
		$author$project$Toolbar$displayModeStyle(disabled_),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Container Display')
					])),
				A4(
				$author$project$Toolbar$viewRadioButton,
				'Black Box',
				$author$project$AppModel$SwitchDisplay(
					$author$project$Model$Container($author$project$Model$BlackBox)),
				checked1,
				disabled_),
				A4(
				$author$project$Toolbar$viewRadioButton,
				'White Box',
				$author$project$AppModel$SwitchDisplay(
					$author$project$Model$Container($author$project$Model$WhiteBox)),
				checked2,
				disabled_),
				A4(
				$author$project$Toolbar$viewRadioButton,
				'Unboxed',
				$author$project$AppModel$SwitchDisplay(
					$author$project$Model$Container($author$project$Model$Unboxed)),
				checked3,
				disabled_)
			]));
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$Config$date = 'Aug 25, 2025';
var $author$project$Config$footerFontSize = 13;
var $author$project$Toolbar$footerStyle = _List_fromArray(
	[
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$footerFontSize) + 'px'),
		A2($elm$html$Html$Attributes$style, 'color', 'lightgray')
	]);
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $author$project$Toolbar$linkStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'color', 'lightgray')
	]);
var $author$project$Config$version = '0.2-snapshot';
var $author$project$Toolbar$viewFooter = A2(
	$elm$html$Html$div,
	$author$project$Toolbar$footerStyle,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text($author$project$Config$version)
				])),
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text($author$project$Config$date)
				])),
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Source: '),
					A2(
					$elm$html$Html$a,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$href('https://github.com/dmx-systems/dm6-elm')
							]),
						$author$project$Toolbar$linkStyle),
					_List_fromArray(
						[
							$elm$html$Html$text('GitHub')
						]))
				])),
			A2(
			$elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('https://dmx.berlin')
					]),
				$author$project$Toolbar$linkStyle),
			_List_fromArray(
				[
					$elm$html$Html$text('DMX Systems')
				]))
		]));
var $author$project$Model$Back = {$: 'Back'};
var $author$project$Config$homeMapName = 'DM6 Elm';
var $author$project$ModelAPI$isHome = function (model) {
	return !$author$project$ModelAPI$activeMap(model);
};
var $author$project$Toolbar$getMapName = function (model) {
	if ($author$project$ModelAPI$isHome(model)) {
		return $author$project$Config$homeMapName;
	} else {
		var _v0 = A2(
			$author$project$ModelAPI$getTopicInfo,
			$author$project$ModelAPI$activeMap(model),
			model);
		if (_v0.$ === 'Just') {
			var topic = _v0.a;
			return $author$project$ModelAPI$getTopicLabel(topic);
		} else {
			return '??';
		}
	}
};
var $author$project$Toolbar$mapNavStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'margin-top', '20px'),
		A2($elm$html$Html$Attributes$style, 'margin-bottom', '12px')
	]);
var $author$project$Toolbar$mapTitleStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'font-size', '36px'),
		A2($elm$html$Html$Attributes$style, 'font-weight', 'bold'),
		A2($elm$html$Html$Attributes$style, 'vertical-align', 'top'),
		A2($elm$html$Html$Attributes$style, 'margin-left', '12px')
	]);
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$IconMenuAPI$viewIcon = F2(
	function (iconName, size) {
		var _v0 = A2($elm$core$Dict$get, iconName, $feathericons$elm_feather$FeatherIcons$icons);
		if (_v0.$ === 'Just') {
			var icon = _v0.a;
			return A2(
				$feathericons$elm_feather$FeatherIcons$toHtml,
				_List_Nil,
				A2($feathericons$elm_feather$FeatherIcons$withSize, size, icon));
		} else {
			return $elm$html$Html$text('??');
		}
	});
var $author$project$Toolbar$viewMapNav = function (model) {
	var backDisabled = $author$project$ModelAPI$isHome(model);
	return A2(
		$elm$html$Html$div,
		$author$project$Toolbar$mapNavStyle,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$AppModel$Nav($author$project$Model$Back)),
						$elm$html$Html$Attributes$disabled(backDisabled)
					]),
				_List_fromArray(
					[
						A2($author$project$IconMenuAPI$viewIcon, 'arrow-left', 20)
					])),
				A2(
				$elm$html$Html$span,
				$author$project$Toolbar$mapTitleStyle,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Toolbar$getMapName(model))
					]))
			]));
};
var $author$project$Toolbar$viewMonadDisplay = function (model) {
	var displayMode = function () {
		var _v4 = $author$project$ModelAPI$getSingleSelection(model);
		if (_v4.$ === 'Just') {
			var _v5 = _v4.a;
			var topicId = _v5.a;
			var mapId = _v5.b;
			return A3($author$project$ModelAPI$getDisplayMode, topicId, mapId, model.maps);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}();
	var _v0 = function () {
		if ((displayMode.$ === 'Just') && (displayMode.a.$ === 'Monad')) {
			if (displayMode.a.a.$ === 'LabelOnly') {
				var _v2 = displayMode.a.a;
				return _Utils_Tuple3(true, false, false);
			} else {
				var _v3 = displayMode.a.a;
				return _Utils_Tuple3(false, true, false);
			}
		} else {
			return _Utils_Tuple3(false, false, true);
		}
	}();
	var checked1 = _v0.a;
	var checked2 = _v0.b;
	var disabled_ = _v0.c;
	return A2(
		$elm$html$Html$div,
		$author$project$Toolbar$displayModeStyle(disabled_),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Monad Display')
					])),
				A4(
				$author$project$Toolbar$viewRadioButton,
				'Label Only',
				$author$project$AppModel$SwitchDisplay(
					$author$project$Model$Monad($author$project$Model$LabelOnly)),
				checked1,
				disabled_),
				A4(
				$author$project$Toolbar$viewRadioButton,
				'Detail',
				$author$project$AppModel$SwitchDisplay(
					$author$project$Model$Monad($author$project$Model$Detail)),
				checked2,
				disabled_)
			]));
};
var $author$project$Search$FocusInput = {$: 'FocusInput'};
var $author$project$Search$Input = function (a) {
	return {$: 'Input', a: a};
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$SearchAPI$searchInputStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'width', '100px')
	]);
var $author$project$SearchAPI$viewSearchInput = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Search')
					])),
				A2(
				$elm$html$Html$input,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value(model.search.text),
							$elm$html$Html$Events$onInput(
							A2($elm$core$Basics$composeL, $author$project$AppModel$Search, $author$project$Search$Input)),
							$elm$html$Html$Events$onFocus(
							$author$project$AppModel$Search($author$project$Search$FocusInput))
						]),
					$author$project$SearchAPI$searchInputStyle),
				_List_Nil)
			]));
};
var $author$project$Toolbar$buttonStyle = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'font-family', $author$project$Config$mainFont),
		A2(
		$elm$html$Html$Attributes$style,
		'font-size',
		$elm$core$String$fromInt($author$project$Config$toolbarFontSize) + 'px')
	]);
var $author$project$Toolbar$viewToolbarButton = F4(
	function (label, msg, requireSelection, model) {
		var hasNoSelection = $elm$core$List$isEmpty(model.selection);
		var buttonAttr = function () {
			if (requireSelection) {
				return _List_fromArray(
					[
						$author$project$Utils$stopPropagationOnMousedown($author$project$AppModel$NoOp),
						$elm$html$Html$Attributes$disabled(hasNoSelection)
					]);
			} else {
				return _List_Nil;
			}
		}();
		return A2(
			$elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(msg)
					]),
				_Utils_ap(buttonAttr, $author$project$Toolbar$buttonStyle)),
			_List_fromArray(
				[
					$elm$html$Html$text(label)
				]));
	});
var $author$project$Toolbar$viewToolbar = function (model) {
	return A2(
		$elm$html$Html$div,
		$author$project$Toolbar$toolbarStyle,
		_List_fromArray(
			[
				$author$project$Toolbar$viewMapNav(model),
				$author$project$SearchAPI$viewSearchInput(model),
				A4($author$project$Toolbar$viewToolbarButton, 'Add Topic', $author$project$AppModel$AddTopic, false, model),
				A4(
				$author$project$Toolbar$viewToolbarButton,
				'Edit',
				$author$project$AppModel$Edit($author$project$Model$EditStart),
				true,
				model),
				A4(
				$author$project$Toolbar$viewToolbarButton,
				'Choose Icon',
				$author$project$AppModel$IconMenu($author$project$IconMenu$Open),
				true,
				model),
				$author$project$Toolbar$viewMonadDisplay(model),
				$author$project$Toolbar$viewContainerDisplay(model),
				A4($author$project$Toolbar$viewToolbarButton, 'Hide', $author$project$AppModel$Hide, true, model),
				A4(
				$author$project$Toolbar$viewToolbarButton,
				'Fullscreen',
				$author$project$AppModel$Nav($author$project$Model$Fullscreen),
				true,
				model),
				A4($author$project$Toolbar$viewToolbarButton, 'Delete', $author$project$AppModel$Delete, true, model),
				$author$project$Toolbar$viewFooter
			]));
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$browser$Browser$Document,
		'DM6 Elm',
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_Utils_ap($author$project$MouseAPI$mouseHoverHandler, $author$project$Main$appStyle),
				_Utils_ap(
					_List_fromArray(
						[
							$author$project$Toolbar$viewToolbar(model),
							A3(
							$author$project$MapRenderer$viewMap,
							$author$project$ModelAPI$activeMap(model),
							-1,
							model)
						]),
					_Utils_ap(
						$author$project$SearchAPI$viewResultMenu(model),
						$author$project$IconMenuAPI$viewIconMenu(model)))),
				A2(
				$elm$html$Html$div,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('measure')
						]),
					$author$project$Main$measureStyle),
				_List_fromArray(
					[
						$elm$html$Html$text(model.measureText),
						A2($elm$html$Html$br, _List_Nil, _List_Nil)
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{init: $author$project$Main$init, subscriptions: $author$project$MouseAPI$mouseSubs, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));