var plus = function(list){return list[0] + list[1]};
var minus = function(list){return list[0] - list[1]};
var times = function(list){return list[0] * list[1]};
var le = function(list){return list[0] <= list[1]};
var fact_6 = (function (n_7) { var t_37 = [n_7, 1];
var cond_20 = le (t_37);
if (cond_20)
{ return (1); }
else { var t_32 = [n_7, 1];
var arg_27 = minus (t_32);
var e_23 = fact_6 (arg_27);
var t_25 = [n_7, e_23];
return (times (t_25)); }; });
var result_0 = fact_6 (10);
console.log(result_0);