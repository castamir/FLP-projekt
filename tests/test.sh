#!/bin/bash
 

  
function testRegexp {
	echo "Testing '$1' and '$2' against /etc/passwd"

	rm -f *.output
	
    ../xsurov03 $1 /etc/passwd > given.output
    grep $2 /etc/passwd > expected.output
    
    DIFF=$(diff expected.output given.output) 
	if [ "$DIFF" != "" ] 
	then
	    echo "FAILED"
	    echo ""
	    printf "%s" "$DIFF"
	else
	    echo "SUCCESS"
	fi
	echo ""
}

testRegexp "xpauli00" "xpauli00"
testRegexp "xpau..00" "xpau..00"
testRegexp "xpaul.*" "xpaul.*"
testRegexp "xpaul.*FIT" "xpaul.*FIT"
 
