var assertEqual = function(x, y) {
    if(x===y) {
        console.log(".");
    } else {
        console.log("Test Failed! " + x + " was not equal to: " + y);
    }
};



assertEqual(1,1);
assertEqual(1,2);
assertEqual(1,1);
