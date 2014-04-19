#!/bin/bash
 

  
function testRegexp {
	echo "Testing '$1' against source.data"

	rm -f *.output
	
    ../xsurov03 "$1" source.data > given.output
    grep -E "$1" source.data > expected.output
    
    DIFF=$(diff expected.output given.output) 
	if [ "$DIFF" != "" ] 
	then
	    echo "FAILED"
	    printf "\n%s" "$DIFF"
	else
	    echo "SUCCESS"
	fi
	echo ""
}

testRegexp "xpauli00"
testRegexp "xpau..00"
testRegexp "xpaul.*"
testRegexp "xpaul.*FIT"
testRegexp "xpa.*FIT.*1r"
testRegexp "FIT.*MIS.*1r"
testRegexp "MIS.*[3-9]r"
testRegexp "xpauli"
testRegexp "xpauli(00)"
testRegexp "xpauli(00)*"
testRegexp "xpauli(00)+"
testRegexp "xpauli(00)?"
testRegexp "[xpauli]+(00)?"
testRegexp "[xpauli][xpauli][xpauli][xpauli][xpauli][xpauli](00)"
testRegexp "[xpauli][xpauli]+[xpauli](00)"
testRegexp "xpau[xpauli]([xpauli]*)*[xpauli](00)"
testRegexp "xpau[xpauli]([xpauli]*)*[xpauli]([0-0][^a-zA-Z1-9])"
testRegexp "[^x]*"
 
