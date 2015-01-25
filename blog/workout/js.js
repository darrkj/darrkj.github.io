 function isEven(n) {return n % 2 == 1}
 
 (2 ^ 32) - 1 
 4294967295
 
 var val = 27; 
 
 x = true;
 while (x) {
   
 }
 
 function flip(n) {
   if(n == 0) {
     return '000000000000000000000000000000000'
   } else if (n == 1) {
     return '000000000000000000000000000000001'
   } else {
     var y = Math.log(n) / Math.log(2);
     var z = y % 1;
     var x = Math.floor(y);
     var a = '';
   }

   if(z > 0) {
     a = '1';
   } else {
     a = '0';
   }
   for(var i = 0; i < 32; ++i) {
     if(i < x) {
       a = '1' + a;
     } else {
       a = '0' + a
     }//if (Math.floor(z)z > 1)
   }
 return a
 }
 

  function flip(n) {
  var a = '';
  var x = n;
  while (x > 0) {
    x = x / 2;
    if (x % 1 == 0) {
      a = '0' + a;
    } else {
      a = '1' + a;
    }
    x = Math.floor(x);
  }


  var g = 32 - a.length;
  for(var i = 0; i < g; ++i) {
    a = '0' + a;
  }
    print(a.length);
  var b = '';
  var c = 0;
  for(var i = 0; i < 32; ++i) {
    if (a[i] == '0') {
      b = b + '1';
    } else {
      b = b + '0';
    }
  }
  print(a);
  print(b)
  for(var i = 0; i < 32; ++i) {
    if (b[i] == '1') {
      c += Math.pow(2, i);
    }
  }
  return c;
}

1111111111111111111111111111111
1111111111111111111111111111111
01111111111111111111111111111111








  function flip(n) {
  var a = '';
  var x = n;
  while (x > 0) {
    x = x / 2;
    if (x % 1 == 0) {
      a = '0' + a;
    } else {
      a = '1' + a;
    }
    x = Math.floor(x);
  }

  var g = 32 - a.length;
  for(var i = 0; i < g; ++i) {
    a = '0' + a;
  }
  print(a);
  var b = '';
  var c = 0;
  for(var i = 0; i < 32; ++i) {
    if (a[i] == '0') {
      b = b + '1';
    } else {
      b = b + '0';
    }
  }
  print(b)
  for(var i = 0; i < 33; ++i) {
    if (b[32-i] == '1') {
      c += Math.pow(2, i-1);
      print(c)
    }
  }
  return c;
}




function processData(input) {
  var inp = input.split("\n");
  function flip(n) {
  var a = '';
  var x = n;
  while (x > 0) {
    x = x / 2;
    if (x % 1 == 0) {
      a = '0' + a;
    } else {
      a = '1' + a;
    }
    x = Math.floor(x);
  }

  var g = 32 - a.length;
  for(var i = 0; i < g; ++i) {
    a = '0' + a;
  }
  var b = '';
  var c = 0;
  for(var i = 0; i < 32; ++i) {
    if (a[i] == '0') {
      b = b + '1';
    } else {
      b = b + '0';
    }
  }
  for(var i = 0; i < 33; ++i) {
    if (b[32-i] == '1') {
      c += Math.pow(2, i-1);
    }
  }
  return c;
}
    var tmp;
for(var i = 1; i <inp.length; ++i) {
    tmp = inp[i];
    console.log(flip(tmp));
    //console.log(tmp);
}
//    console.log(inp);
}


  var x;
  for(var i = 1; i < inp.length; ++i) {
    x[i-1] = inp[i];
  }


function processData(input) {
  var inp = input.split("\n");
  var x;
  for(var i = 1; i < inp.length; ++i) {
    x[i-1] = inp[i];
  }

var fibs = [0, 1, 1, 2, 3, 5, 8, 13];
var max = 13;
while(max < 1000000) {
  fibs[fibs.length + 1] = fibs[fibs.length] + fibs[fibs.length - 1];
  max = fibs[fibs.length + 1]
}

console.log(fibs)
//function fib(n)



  for(var i = 1; i < inp.length; ++i) {
    console.log(flip(inp[i]));
  }
}






function processData(input) {
  var inp = input.split("\n")[1];
  inp = inp.split(" "); 
  var l = inp.length - 1;
  var tmp = inp[l];
    console.log(tmp);
  for(var i = 0; i < l; ++i) {
      console.log(inp[l-i-1]);
    if(tmp < inp[l-i-1]) {
        inp[2] = 5;//inp[l-i-1];
        console.log(inp);
    }
    //console.log(inp[i]);
  }
} 
