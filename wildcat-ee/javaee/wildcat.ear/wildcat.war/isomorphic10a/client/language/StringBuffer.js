/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 







//>	@class	StringBuffer
//
//		Use instances of this class to concatenate strings rather than using the normal "this"+"that" methodology.
//		For large sets of strings, this can be up to an order of mangintude faster!
//
//		You can use this class in two ways:
//			1) if you have a static and fairly small set of things to concatenate, call statically
//					alert(StringBuffer.concat("this"," ","that ","and the other"))
//				yields:		"This that and the other"
//
//			2) if you have a loop or more complex logic, create s StringBuffer instance and append to that,
//				then do a buffer.toString() on the results (or do something like an alert() or document.write()
//				that does a toString() for you:
//				
//                  <pre>
//					var buffer = StringBuffer.newInstance();
//					for (var i = 0; i < 10; i++) {
//						buffer.append(i, " ");
//					}
//					alert(buffer)
//				yields:	"0 1 2 3 4 5 6 7 8 9 "
//              </pre>
//<
isc.ClassFactory.defineClass("StringBuffer");
// nickname
isc.SB = isc.StringBuffer;

isc.StringBuffer.addClassProperties({
    // For efficiency we re-use StringBuffers (created lazily when needed).
    _bufferPool:[],
    // upper limit on the number of outstanding buffers to be re-used
    _maxPoolSize:50
});


isc.StringBuffer.addProperties({
    
	maxStreamLength : (isc.Browser.isIE6 ? 1000 : 100000),

    // Don't add props passed in to the SB on create - not supported and this is slightly
    // more efficient
    addPropertiesOnCreate:false
    
});
isc.StringBuffer.addMethods({

    
//>	@method		stringBuffer.init()	(A)
// Initialize the string buffer
//		@group	concat
//
//		@param	[a,b,c]	(object)	properties for the buffer instance
//<
init: function () {
	// create the stream array
	this._stream = [];
},

//>	@method		stringBuffer.append()
// Append all arguments to the string buffer as strings
//		@group	concat
//
//		@param	[arguments]	(string)	strings to append to the buffer
//<
append : function (arg1,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) {
    
    
    // Set up local variable to use - this is a very small amount quicker than always referencing
    // this._stream directly
    var theStream = this._stream,  // by reference - manipulating this manipulates this._stream
        strings,
        undef;
   
    // If we are passed an array, it's quickest to manually add it slot by slot
   	if (arg1 != null && arg1.constructor.__nativeType == 2) {

        

        var length = arg1.length;
        if (length <= 30) {       
            var length = theStream.length;
            for (var i = 0; i < arg1.length; i++) {
                theStream[length++] = arg1[i];
            }
        } else {        
            theStream[theStream.length] = arg1.join(isc.emptyString)
        }


    // If we are not passed an array it's quickest to iterate through the arguments and add them
    // to this._stream.
    // We don't have arguments.join(), and adding it is too expensive
    } else {
        if (Z === undef && Y === undef && X === undef) {

            
            if (arg1 != null) theStream[theStream.length] = arg1;
            if (A != null) theStream[theStream.length] = A
            if (B != null) theStream[theStream.length] = B
            if (C != null) theStream[theStream.length] = C
            if (D != null) theStream[theStream.length] = D
            if (E != null) theStream[theStream.length] = E
            if (F != null) theStream[theStream.length] = F
            if (G != null) theStream[theStream.length] = G
            if (H != null) theStream[theStream.length] = H
            if (I != null) theStream[theStream.length] = I
            if (J != null) theStream[theStream.length] = J
            if (K != null) theStream[theStream.length] = K
            if (L != null) theStream[theStream.length] = L
            if (M != null) theStream[theStream.length] = M
            if (N != null) theStream[theStream.length] = N
            if (O != null) theStream[theStream.length] = O
            if (P != null) theStream[theStream.length] = P
            if (Q != null) theStream[theStream.length] = Q
            if (R != null) theStream[theStream.length] = R
            if (S != null) theStream[theStream.length] = S
            if (T != null) theStream[theStream.length] = T
            if (U != null) theStream[theStream.length] = U
            if (V != null) theStream[theStream.length] = V
            if (W != null) theStream[theStream.length] = W 

        // If we were passed more than 27 args, look at the arguments object
        
        } else {
            strings = arguments;
            for (var i = 0, l = strings.length; i < l; i++) {
                theStream[theStream.length] = strings[i]
            }
        }
    }
    
	// if we're holding on to too many string instances, collapse them into one instance
    // This is because IE slows down in general when a lot of large objects are sitting in
    // memory
    if (theStream.length > this.maxStreamLength) {
        theStream[0] = theStream.join(isc.emptyString); 
        //isc.Log.logWarn("collapsing stream: " + theStream[0].substring(0, 80));
        theStream.length = 1;
    }
    return this;
},


appendNumber : function (number, length) {
    var stream = this._stream;
    if (length == null) {
        length = 5;
        var numberCopy = number;
        if (numberCopy < 0) {
            numberCopy = 0 - numberCopy;
            // add one for the "-" char
            length += 1;
        }
        // If it will take up more than 5 slots, determine how many it needs
        if (numberCopy >= 100000) {
            numberCopy = numberCopy / 100000;
            while (numberCopy >= 1) {
                length += 1;
                numberCopy = numberCopy / 10;
            }
        }
    }
    isc._fillNumber(stream, number, stream.length, length);
},

clear : function () {
    this._stream.length = 0;
},


// Can be called when a stringBuffer is no longer required - gets added to the pool to be reused
// Also returns the buffer's contents
release : function (noReturnValue) {
    
    var SB = isc.SB, pool = SB._bufferPool,
        string = noReturnValue ? null : this.toString();
    if (pool.length < SB._maxPoolSize) {

        // Clear it out and put it into the pool 
        this.clear();
        pool[pool.length] = this;
    }
    if (!noReturnValue) return string;
},


getArray : function () {
    return this._stream;
}

});

//>	@method		stringBuffer.toString()
//		@group	concat
// 			Return all of the appended strings as a single string.
// 			Added manually here since doing it with addMethods doesn't work because toString is not
// 			enumerable.
//
//		@return	(string)	a single concatenated string
//<
isc.StringBuffer.getPrototype().toString = function () {
    //if (isc.Browser.isMoz) isc.SB._checkArray(this._stream);
	return this._stream.join(isc.emptyString);
}

isc.StringBuffer._joinFunc = Array.prototype.join;
isc.StringBuffer.addClassMethods({

// Override create() - if we've already created a stringBuffer that's no longer being
// used, reuse it.
create : function () {
    var pool = this._bufferPool,
        poolLength = pool.length;
    if (poolLength > 0) {
        var buffer = pool[poolLength -1];
        pool.length = poolLength -1;
        return buffer;
    } else {
        // standard creation.
        return isc.Class.create.apply(this);
    }
},    



//>	@method		StringBuffer.concat()
//		@group	concat
// 			Static method that will return a string composed of the arguments passed in
//
//		@return	(string)	a single concatenated string
//<
_joinBuffer : [],
concat : function (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
                   a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) 
{
    
    
    var undef,
        returnString;
    if (isc.Browser.isIE && x === undef && y === undef && z === undef) {
        var buffer = this._joinBuffer;
        buffer.length = 0;

        
        if (A != null) buffer[buffer.length] = A;
        if (B != null) buffer[buffer.length] = B;
        if (C != null) buffer[buffer.length] = C;
        if (D != null) buffer[buffer.length] = D;
        if (E != null) buffer[buffer.length] = E;        
        if (F != null) buffer[buffer.length] = F;
        if (G != null) buffer[buffer.length] = G;
        if (H != null) buffer[buffer.length] = H;
        if (I != null) buffer[buffer.length] = I;
        if (J != null) buffer[buffer.length] = J;
        if (K != null) buffer[buffer.length] = K;  
        if (L != null) buffer[buffer.length] = L;
        if (M != null) buffer[buffer.length] = M;        
        if (N != null) buffer[buffer.length] = N;
        if (O != null) buffer[buffer.length] = O;
        if (P != null) buffer[buffer.length] = P;
        if (Q != null) buffer[buffer.length] = Q;
        if (R != null) buffer[buffer.length] = R;
        if (S != null) buffer[buffer.length] = S;
        if (T != null) buffer[buffer.length] = T;
        if (U != null) buffer[buffer.length] = U;
        if (V != null) buffer[buffer.length] = V;
        if (W != null) buffer[buffer.length] = W;
        if (X != null) buffer[buffer.length] = X;
        if (Y != null) buffer[buffer.length] = Y;
        if (Z != null) buffer[buffer.length] = Z;
        if (a != null) buffer[buffer.length] = a;
        if (b != null) buffer[buffer.length] = b;
        if (c != null) buffer[buffer.length] = c;
        if (d != null) buffer[buffer.length] = d;
        if (e != null) buffer[buffer.length] = e;
        if (f != null) buffer[buffer.length] = f;
        if (g != null) buffer[buffer.length] = g;
        if (h != null) buffer[buffer.length] = h;
        if (i != null) buffer[buffer.length] = i;
        if (j != null) buffer[buffer.length] = j;
        if (k != null) buffer[buffer.length] = k;
        if (l != null) buffer[buffer.length] = l;
        if (m != null) buffer[buffer.length] = m;
        if (n != null) buffer[buffer.length] = n;
        if (o != null) buffer[buffer.length] = o;
        if (p != null) buffer[buffer.length] = p;
        if (q != null) buffer[buffer.length] = q;
        if (r != null) buffer[buffer.length] = r;
        if (s != null) buffer[buffer.length] = s;
        if (t != null) buffer[buffer.length] = t;
        if (u != null) buffer[buffer.length] = u;
        if (v != null) buffer[buffer.length] = v;
        if (w != null) buffer[buffer.length] = w;
        if (x != null) buffer[buffer.length] = x;
        if (y != null) buffer[buffer.length] = y;
        if (z != null) buffer[buffer.length] = z;
        
        returnString = buffer.join(isc.emptyString);
    } else {   
	    arguments.join = this._joinFunc;
        returnString = arguments.join(isc.emptyString);
    }
    
    return returnString;
}

});

