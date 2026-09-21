function x = ordercal( a,b )
if a > b
    x = log(a/b)/log(2);
else 
    x = log(b/a)/log(2);
end
