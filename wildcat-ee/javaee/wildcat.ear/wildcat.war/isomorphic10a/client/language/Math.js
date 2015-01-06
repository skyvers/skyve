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

//
// Math helpers
//
isc.Math = {
    clamp : function (value, min, max) {
        if (value < min) return min;
        if (value > max) return max;
        return value;
    },
    // When called with two parameters `a' and `b', selects a random integer between `a' and `b'
    // inclusive, drawn uniformly.
    // When called with a single parameter `a', selects a random integer between 0 and `a' inclusive,
    // drawn uniformly.
    // @param a (int)
    // @param [b] (int)
    // @return (int)
    
    random : function (a, b) {
        
        if (b == null) {
            
            return Math.floor(Math.random() * (a + 1));
        } else {
            
            return Math.floor(Math.random() * (b - a + 1)) + a;
        }
    },

    _hexStringify : function (number, totalDigits) {
        var str = number.toString(16);
        if (str.length < totalDigits) str = isc.NumberUtil._getZeroString(totalDigits - str.length) + str;
        return str;
    },
    // Generate an RFC 4122-compliant UUID according to section 4.4. Algorithms for Creating a
    // UUID from Truly Random or Pseudo-Random Numbers.
    
    randomUUID : function () {
        var uint16s;
        if (window.Uint16Array && window.crypto && window.crypto.getRandomValues) {
            uint16s = new window.Uint16Array(8);
            window.crypto.getRandomValues(uint16s);
        } else {
            uint16s = new Array(8);
            var now = new Date().getTime();
            for (var i = 0; i < uint16s.length; ++i) {
                uint16s[i] = (now ^ (Math.random() * 65536)) & 0xFFFF;
            }
        }

        uint16s[3] = (uint16s[3] & 0x0FFF) | 0x4000;

        // Set the two most significant bits of clock_seq_hi_and_reserved to 0 and 1, respectively.
        uint16s[4] = (uint16s[4] & 0xBFFF) | 0x8000;

        return (this._hexStringify(uint16s[0], 4) + this._hexStringify(uint16s[1], 4) + "-" +
                this._hexStringify(uint16s[2], 4) + "-" +
                this._hexStringify(uint16s[3], 4) + "-" +
                this._hexStringify(uint16s[4], 4) + "-" +
                this._hexStringify(uint16s[5], 4) + this._hexStringify(uint16s[6], 4) + this._hexStringify(uint16s[7], 4)).toUpperCase();
    },

    // Generates a random string of length `len' from characters in a specified alphabet.
    // @param len (int) the length of the generated random string.
    // @param alphabet (String | int) when a String, each character of the string has an equal
    // probability of being selected as a character of the generated random string. When an
    // integer less than or equal to 36, the first `alphabet' characters of "0123456789abcdefghijklmnopqrstuvwxyz"
    // become the alphabet.
    randomString : function (len, alphabet) {
        var arr = new Array(len);
        var alphabetLen;
        if (isc.isA.Number(alphabet)) {
            alphabetLen = alphabet;
            alphabet = "0123456789abcdefghijklmnopqrstuvwxyz".substring(0, alphabetLen);
        } else {
            alphabetLen = alphabet.length;
        }
        for (var i = 0; i < len; ++i) {
            arr[i] = alphabet[this.random(alphabetLen - 1)];
        }
        return arr.join("");
    },

    _signum : function (x) {
        return (x < 0 ? -1 : (x > 0 ? 1 : 0));
    },

    // Calculate sqrt(a^2 + b^2) without overflow or underflow
    // Note: Firefox 27.0+ supports Math.hypot() from EcmaScript 6: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot
    _hypot : function (a, b) {
        a = Math.abs(a);
        b = Math.abs(b);
        if (a > b) {
            return a * Math.sqrt(1 + b * b / a / a);
        } else if (b != 0) {
            return b * Math.sqrt(1 + a * a / b / b);
        } else {
            return a;
        }
    },

    // Calculates the shortest Euclidean distance from a test point (x3, y3) to the line between
    // start point (x1, y1) and end point (x2, y2).
    euclideanDistanceToLine : function (x1, y1, x2, y2, x3, y3) {
        // http://web.archive.org/web/20080704103329/http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/

        var dx = x2 - x1,
            dy = y2 - y1;

        var uDenom = dx * dx + dy * dy;
        // If the line's endpoints are coincident, then just return the Euclidean distance from
        // the test point to the start point.
        if (uDenom <= 0.00001) {
            return this.euclideanDistance(x1, y1, x3, y3);
        }

        var u = ((x3 - x1) * (x2 - x1) + (y3 - y1) * (y2 - y1)) / uDenom;

        if (u < 0) {
            return this.euclideanDistance(x1, y1, x3, y3);
        } else if (u > 1) {
            return this.euclideanDistance(x2, y2, x3, y3);
        } else {
            // Actually compute the point of intersection.
            var x = x1 + u * dx,
                y = y1 + u * dy;

            return this.euclideanDistance(x, y, x3, y3);
        }
    },

    // Calculates the Euclidean distance between two points.
    euclideanDistance : function (x1, y1, x2, y2) {
        if (arguments.length == 2) {
            // Assume that the two given arguments are points.
            var p1 = x1,
                p2 = y1;
            x1 = p1[0];
            y1 = p1[1];
            x2 = p2[0];
            y2 = p2[1];
        }
        return this._hypot((x1 - x2), (y1 - y2));
    },

    // Trigonometry
    // ---------------------------------------------------------------------------------------
    _radPerDeg: Math.PI / 180,

    //> @classMethod math.toRadians()
    // Converts an angle in degrees to radians.
    // @param angle (double) the angle in degrees.
    // @return (double) the angle in radians.
    //<
    toRadians : function (angle) {
        return angle * this._radPerDeg;
    },

    //> @classMethod math.cosdeg()
    // Calculates the cosine of the given angle in degrees.
    // @param angle (double) the angle in degrees.
    // @return (double) the cosine of the given angle.
    //<
    cosdeg : function (angle) {
        return Math.cos(angle * this._radPerDeg);
    },

    //> @classMethod math.sindeg()
    // Calculates the sine of the given angle in degrees.
    // @param angle (double) the angle in degrees.
    // @return (double) the sine of the given angle.
    //<
    sindeg : function (angle) {
        return Math.sin(angle * this._radPerDeg);
    },

    // Linear Algebra
    // ---------------------------------------------------------------------------------------

    // Calculates the dot product of two vectors. To be well formed, the two vectors must have
    // the same array length (dimension).
    _dot : function (u, v) {
        var ret = 0;
        for (var i = 0; i < u.length; ++i) {
            ret += u[i] * v[i];
        }
        return ret;
    },

    // Given a matrix A (that is an Array of Arrays of Numbers) returns a new matrix that is the matrix
    // multiplication of the transpose of A times A.  If A has m rows and n columns then the
    // new matrix will have n rows and n columns.
    _dotAtA : function (A) {
        var m = A.length,
            n = A[0].length,
            AtA = new Array(n);

        for (var i = n; i--; ) {
            AtA[i] = new Array(n);
        }

        for (var i = n; i--; ) {
            var AtAi = AtA[i];
            for (var j = i; j < n; ++j) {
                var sum = 0;
                for (var k = m; k--; ) {
                    var Ak = A[k];
                    sum += Ak[i] * Ak[j];
                }
                AtAi[j] = AtA[j][i] = sum;
            }
        }
        return AtA;
    },

    // Given a matrix A, that is an Array of Arrays of Numbers, and a vector b, that is an Array of Numbers,
    // return a new vector that is the matrix multiplication of A times b.  If A has m rows and n columns,
    // then b is expected to have length n, and the returned vector will have length m.
    _dotAtb : function (A, b) {
        if (A.length != b.length) {
            return null;
        }

        var m = A[0].length, n = b.length,
            Atb = new Array(m);

        for (var i = m; i--; ) {
            var sum = 0;
            for (var j = n; j--; ) {
                sum += A[j][i] * b[j];
            }
            Atb[i] = sum;
        }
        return Atb;
    },

    // Calculates the Cholesky decomposition of a symmetric, positive-definite matrix A.
    // A must be an Array of Arrays of Numbers.  The return value is the unique,
    // lower triangular matrix L such that A = L * Lt.  If A has n rows and n columns
    // (it must have equal number of rows and columns in order to be symmetric), then the
    // returned matrix L will also have n rows and n columns.
    // See:  http://en.wikipedia.org/wiki/Cholesky_decomposition
    _cholesky : function (A) {
        if (A.length != A[0].length) {
            // The matrix A is apparently not symmetric, so return null.
            return null;
        }

        var n = A.length,
            L = isc.Math._createMatrix(n, n);

        for (var j = 0; j < n; ++j) {
            var Lj = L[j],
                sum = 0;

            for (var k = 0; k < j; ++k) {
                var Ljk = Lj[k];
                sum += Ljk * Ljk;
            }

            if (A[j][j] - sum < 0) {
                // The matrix A must not have been positive-definite.  In this case
                // the matrix has no Cholesky decomposition, so return null.
                return null;
            }

            var Ljj = Lj[j] = Math.sqrt(A[j][j] - sum);

            for (var i = j + 1; i < n; ++i) {
                var Li = L[i],
                    sum = 0;
                for (var k = 0; k < j; ++k) {
                    sum += Li[k] * Lj[k];
                }
                Li[j] = (A[i][j] - sum) / Ljj;
            }
        }

        return L;
    },

    // Return the transpose of a matrix A (an Array of Arrays of Numbers).  The transpose
    // matrix will have the same number of rows as A has columns and the same
    // number of columns as A has rows.
    _transpose : function (A) {
        var m = A.length, n = A[0].length,
            At = new Array(n);
        for (var i = n; i--; ) {
            At[i] = new Array(m);
        }
        for (var i = n; i--; ) {
            var Ati = At[i];
            for (var j = m; j--; ) {
                Ati[j] = A[j][i];
            }
        }
        return At;
    },

    // Create a matrix of m x n size as an Array of Arrays with no initial values.
    _createMatrix : function (m, n) {
        var A = new Array(m);
        for (var i = m; i--; ) {
            A[i] = new Array(n);
        }
        return A;
    },

    // Similar to _createMatrix(), but the matrix is filled with zeros.
    _createZeroMatrix : function (m, n) {
        var A = new Array(m);
        for (var i = m; i--; ) {
            var Ai = A[i] = new Array(n);
            for (var j = n; j--; ) {
                Ai[j] = 0;
            }
        }
        return A;
    },

    // Creates a new Array of length n that contains zeros as entries.
    _createZeroVector : function (n) {
        var v = new Array(n);
        for (var i = n; i--; ) {
            v[i] = 0;
        }
        return v;
    },

    // Creates a new matrix (an Array of Arrays) that has identical size and
    // entries as the given matrix A.
    _cloneMatrix : function (A) {
        var m = A.length, n = A[0].length,
            B = new Array(m);
        for (var i = m; i--; ) {
            var Ai = A[i],
                Bi = B[i] = new Array(n);
            for (var j = n; j--; ) {
                Bi[j] = Ai[j];
            }
        }
        return B;
    },

    // Calculate the Moore–Penrose pseudoinverse of a matrix A.
    _pseudoInv : function (A, maxIterations) {
        var svd = isc.Math._svd(A, maxIterations, true, true);
        if (svd != null) {
            var s = svd.s,
                m = s.length;
            for (var i = m; i--; ) {
                s[i] = (s[i] == 0 ? 0 : (1 / s[i]));
            }
            return isc.Math._dotUSVt(svd.V, s, svd.U);
        } else {
            return null;
        }
    },

    // Calculate the singular value decomposition of a matrix A into the product
    // A = U * S * Vt, where U and V are unitary matrices, and S is a
    // diagonal matrix.  The return value is an object with the keys "U" and "V"
    // each mapped to a matrix (an Array of Arrays of Numbers) and the key "s" mapped
    // to an Array of Numbers.  The matrix S in the singular value decomposition can
    // be formed by taking a zero matrix of the appropriate size (see _createZeroMatrix())
    // and filling the diagonal entries of that matrix with the entries of s:
    //
    //     var m = A.length,
    //         n = A[0].length,
    //         svd = isc.Math._svd(A),
    //         s = svd.s,
    //         S = isc.Math._createZeroMatrix(m, n);
    //     for (var i = 0; i < m && i < n; ++i) {
    //         S[i][i] = s[i];
    //     }
    //
    
    _svd : function (A, maxIterations, wantU, wantV, calculateThinSVD) {
        if (maxIterations == null) {
            maxIterations = 50;
        }
        if (wantU == null) {
            wantU = true;
        }
        if (wantV == null) {
            wantV = true;
        }

        var eps = 2.220446049250313e-16; // 2^-52
        var tiny = Number.MIN_VALUE;
        var m = A.length, n = A[0].length;

        if (m < n) {
            var ret = isc.Math._svd(isc.Math._transpose(A), maxIterations, wantV, wantU);
            if (ret != null) {
                var swap = ret.U;
                ret.U = ret.V;
                ret.V = swap;
            }
            return ret;
        }

        var hypot = isc.Math._hypot,
            nu = Math.min(m, n),
            q = (calculateThinSVD ? nu : m),
            p = Math.min(n, m + 1),
            nct = Math.min(m - 1, n),
            nrt = Math.max(0, Math.min(n - 2, m)),
            A = isc.Math._cloneMatrix(A),
            s = new Array(p),
            U = isc.Math._createZeroMatrix(m, q),
            V = isc.Math._createZeroMatrix(n, n),
            e = isc.Math._createZeroVector(n),
            work = isc.Math._createZeroVector(m);

        for (var k = 0, maxK = Math.max(nct, nrt); k < maxK; ++k) {
            if (k < nct) {
                s[k] = 0;
                for (var i = k; i < m; ++i) {
                    s[k] = hypot(s[k], A[i][k]);
                }
                if (s[k] != 0) {
                    if (A[k][k] < 0) {
                        s[k] = -s[k];
                    }
                    for (var i = k; i < m; ++i) {
                        A[i][k] /= s[k];
                    }
                    A[k][k] += 1;
                }
                s[k] = -s[k];
            }
            for (var j = k + 1; j < n; ++j) {
                if (k < nct && s[k] != 0) {
                    // apply the transformation
                    var t = 0;
                    for (var i = k; i < m; ++i) {
                        t += A[i][k] * A[i][j];
                    }
                    t = -t / A[k][k];
                    for (var i = k; i < m; ++i) {
                        A[i][j] += t * A[i][k];
                    }
                }

                // place the kth row of A into e for the subsequent calculation of the row transform
                e[j] = A[k][j];
            }
            if (wantU && k < nct) {
                // place the transformation in U for subsequent back multiplication
                for (var i = k; i < m; ++i) {
                    U[i][k] = A[i][k];
                }
            }
            if (k < nrt) {
                // compute the kth row transformation and place the kth super-diagonal into e[k].
                e[k] = 0;
                for (var i = k + 1; i < n; ++i) {
                    e[k] = hypot(e[k], e[i]);
                }
                if (e[k] != 0) {
                   if (e[k + 1] < 0) {
                       e[k] = -e[k];
                   }
                   for (var i = k + 1; i < n; ++i) {
                       e[i] /= e[k];
                   }
                   e[k + 1] += 1;
                }
                e[k] = -e[k];

                if (k + 1 < m && e[k] != 0) {
                    // apply the transformation
                    for (var i = k + 1; i < m; ++i) {
                        work[i] = 0;
                    }
                    for (var j = k + 1; j < n; ++j) {
                        for (var i = k + 1; i < m; ++i) {
                            work[i] += e[j] * A[i][j];
                        }
                    }
                    for (var j = k + 1; j < n; ++j) {
                        var t = -e[j] / e[k + 1];
                        for (var i = k + 1; i < m; ++i) {
                            A[i][j] += t * work[i];
                        }
                    }
                }
                if (wantV) {
                    // place the transformation in V for subsequent back multiplication
                    for (var i = k + 1; i < n; ++i) {
                        V[i][k] = e[i];
                    }
                }
            }
        }

        // Set up the final bidiagonal matrix of order p
        if (nct < n) {
            s[nct] = A[nct][nct];
        }
        if (m < p) {
            s[p - 1] = 0;
        }
        if (nrt + 1 < p) {
            e[nrt] = A[nrt][p - 1];
        }
        e[p - 1] = 0;

        // If required, generate U
        if (wantU) {
            for (var j = nct; j < q; ++j) {
                for (var i = 0; i < m; ++i) {
                    U[i][j] = 0;
                }
                U[j][j] = 1;
            }
            for (var k = nct - 1; k >= 0; --k) {
                if (s[k] != 0) {
                    for (var j = k + 1; j < q; ++j) {
                        var t = 0;
                        for (var i = k; i < m; ++i) {
                            t += U[i][k] * U[i][j];
                        }
                        t = -t / U[k][k];
                        for (var i = k; i < m; ++i) {
                            U[i][j] += t * U[i][k];
                        }
                    }
                    for (var i = k; i < m; ++i) {
                        U[i][k] = -U[i][k];
                    }
                    U[k][k] += 1;
                    for (var i = 0; i < k - 1; ++i) {
                        U[i][k] = 0;
                    }
                } else {
                    for (var i = 0; i < m; ++i) {
                        U[i][k] = 0;
                    }
                    U[k][k] = 1;
                }
            }
        }

        // If required, generate V
        if (wantV) {
            for (var k = n - 1; k >= 0; --k) {
                if (k < nrt && e[k] != 0) {
                    for (var j = k + 1; j < nu; ++j) {
                        var t = 0;
                        for (var i = k + 1; i < n; ++i) {
                            t += V[i][k] * V[i][j];
                        }
                        t = -t / V[k+1][k];
                        for (var i = k + 1; i < n; ++i) {
                            V[i][j] += t * V[i][k];
                        }
                    }
                }
                for (var i = 0; i < n; ++i) {
                    V[i][k] = 0;
                }
                V[k][k] = 1;
            }
        }

        // Main iteration loop for the singular values.
        var pp = p-1,
            iter = 0;
        while (p > 0) {
            if (iter > maxIterations) {
                return null;
            }

            // Inspect for negligible elements in the s and e arrays.
            // case 1:  s[p] and e[k-1] are negligible and k < p
            // case 2:  s[k] is negligible and k < p
            // case 3:  e[k-1] is negligible, k < p, and s[k], ..., s[p] are not negligible (QR step)
            // case 4:  e[p-1] is negligible (convergence)
            var k, caseNum;
            for (k = p - 2; k >= -1; --k) {
                if (k == -1) {
                    break;
                }
                if (Math.abs(e[k]) <= tiny + eps*(Math.abs(s[k]) + Math.abs(s[k+1]))) {
                    e[k] = 0;
                    break;
                }
            }
            if (k == p - 2) {
                // e[p - 1] is negligible (convergence)
                caseNum = 4;
            } else {
                var ks;
                for (ks = p - 1; ks >= k; --ks) {
                    if (ks == k) {
                        break;
                    }
                    var t = (ks != p ? Math.abs(e[ks]) : 0) +
                            (ks != k + 1 ? Math.abs(e[ks - 1]) : 0);
                    if (Math.abs(s[ks]) <= tiny + eps * t)  {
                        s[ks] = 0;
                        break;
                    }
                }
                if (ks == k) {
                    // e[k-1] is negligible, k < p, and
                    // s[k], ..., s[p] are not negligible => QR step
                    caseNum = 3;
                } else if (ks == p - 1) {
                    // s[p] and e[k-1] are negligible and k < p
                    caseNum = 1;
                } else {
                    // s[k] is negligible and k < p
                    caseNum = 2;
                    k = ks;
                }
            }
            ++k;

            // Perform the task indicated by the exact case:
            switch (caseNum) {
            case 1:
                // Deflate negligible s[p]
                var f = e[p-2];
                e[p-2] = 0;
                for (var j = p - 2; j >= k; --j) {
                    var t = hypot(s[j], f),
                        cs = s[j] / t,
                        sn = f / t;
                    s[j] = t;
                    if (j != k) {
                        f = -sn * e[j-1];
                        e[j-1] = cs * e[j-1];
                    }
                    if (wantV) {
                        for (var i = 0; i < n; ++i) {
                            t = cs * V[i][j] + sn * V[i][p-1];
                            V[i][p-1] = -sn * V[i][j] + cs * V[i][p-1];
                            V[i][j] = t;
                        }
                    }
                }
                break;

            case 2:
                // Split at negligible s(k).
                var f = e[k-1];
                e[k-1] = 0;
                for (var j = k; j < p; ++j) {
                    var t = hypot(s[j], f),
                        cs = s[j] / t,
                        sn = f / t;
                    s[j] = t;
                    f = -sn * e[j];
                    e[j] = cs * e[j];
                    if (wantU) {
                        for (var i = 0; i < m; ++i) {
                            t = cs * U[i][j] + sn * U[i][k-1];
                            U[i][k-1] = -sn * U[i][j] + cs * U[i][k-1];
                            U[i][j] = t;
                        }
                    }
                }
                break;

            case 3:
                // Perform one QR step

                // Calculate the shift.
                var scale = Math.max(
                        Math.abs(s[p-1]),
                        Math.abs(s[p-2]),
                        Math.abs(e[p-2]),
                        Math.abs(s[k]),
                        Math.abs(e[k])),
                    sp = s[p-1] / scale,
                    spm1 = s[p-2] / scale,
                    epm1 = e[p-2] / scale,
                    sk = s[k] / scale,
                    ek = e[k] / scale,
                    b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2,
                    c = sp * epm1 * sp * epm1,
                    shift = 0;
                if (!(b == 0 && c == 0)) {
                   shift = Math.sqrt(b * b + c);
                   if (b < 0) {
                      shift = -shift;
                   }
                   shift = c / (b + shift);
                }
                var f = (sk + sp) * (sk - sp) + shift,
                    g = sk * ek;

                // Chase zeros
                for (var j = k; j < p - 1; ++j) {
                   var t = hypot(f, g),
                       cs = f / t,
                       sn = g / t;
                   if (j != k) {
                      e[j-1] = t;
                   }
                   f = cs * s[j] + sn * e[j];
                   e[j] = cs * e[j] - sn * s[j];
                   g = sn * s[j+1];
                   s[j+1] = cs * s[j+1];
                   if (wantV) {
                       for (var i = 0; i < n; ++i) {
                           t = cs * V[i][j] + sn * V[i][j+1];
                           V[i][j+1] = -sn * V[i][j] + cs * V[i][j+1];
                           V[i][j] = t;
                       }
                   }
                   t = hypot(f, g);
                   cs = f / t;
                   sn = g / t;
                   s[j] = t;
                   f = cs * e[j] + sn * s[j+1];
                   s[j+1] = -sn * e[j] + cs * s[j+1];
                   g = sn * e[j+1];
                   e[j+1] = cs * e[j+1];
                   if (wantU && j < m - 1) {
                       for (var i = 0; i < m; ++i) {
                           t = cs * U[i][j] + sn * U[i][j+1];
                           U[i][j+1] = -sn * U[i][j] + cs * U[i][j+1];
                           U[i][j] = t;
                       }
                   }
                }
                e[p-2] = f;
                ++iter;
                break;

            case 4:
                // Convergence.

                // Make the singular values non-negative
                if (s[k] <= 0) {
                    s[k] = -s[k];
                    if (wantV) {
                        for (var i = 0; i <= pp; ++i) {
                            V[i][k] = -V[i][k];
                        }
                    }
                }

                // Order the singular values.
                for (; k < pp; ++k) {
                    if (s[k] >= s[k+1]) {
                        break;
                    }
                    var t = s[k];
                    s[k] = s[k+1];
                    s[k+1] = t;
                    if (wantV && k < n - 1) {
                        for (var i = 0; i < n; ++i) {
                            t = V[i][k+1]; V[i][k+1] = V[i][k]; V[i][k] = t;
                        }
                    }
                    if (wantU && k < m - 1) {
                        for (var i = 0; i < m; ++i) {
                            t = U[i][k+1]; U[i][k+1] = U[i][k]; U[i][k] = t;
                        }
                    }
                }
                iter = 0;
                --p;
                break;
            } // end of switch
        } // end of loop while p > 0

        return { U: U, s: s, V: V };
    },

    // Takes a matrix U, an array s that defines the diagonal elements of a diagonal matrix S, and
    // a matrix V, and returns the matrix multiplication of U times S times the transpose of V.
    // This method may be used to check the singular value decomposition of a matrix A,
    // but it is also used to calculate the Moore–Penrose pseudoinverse of A (see _pseudoInv()).
    _dotUSVt : function (U, s, V) {
        var m = U.length,
            n = V.length,
            l = Math.min(m, n),
            A = isc.Math._createMatrix(m, n);

        for (var i = m; i--; ) {
            var Ui = U[i], Ai = A[i];
            for (var j = n; j--; ) {
                var sum = 0, Vj = V[j];
                for (var k = l; k--; ) {
                    sum += Ui[k] * s[k] * Vj[k];
                }
                Ai[j] = sum;
            }
        }
        return A;
    },

    _EPSILON : (function () {
        var i = 0;
        while (1.0 + Math.pow(2, -i) > 1.0) {
            ++i;
        }
        return Math.pow(2, -i + 1);
    })(),

    // A constructor function for a complex number.
    _complexNumber : function (real, imag) {
        this.real = real;
        this.imag = imag;
    },

    
    _rpolyState : {
        // arrays of numbers:
        p: [], qp: [], k: [], qk: [], svk: [],
        // numbers:
        sr: 0, si: 0, u: 0, v: 0, a: 0, b: 0, c: 0, d: 0, a1: 0, a2: 0, a3: 0, a6: 0, a7: 0, e: 0,
        f: 0, g: 0, h: 0, szr: 0, szi: 0, lzr: 0, lzi: 0,
        // integers:
        n: 0,
        nn: 0
    },
    _rpoly : function (op, degree) {
        
        var state = isc.Math._rpolyState,
            zeros = [],
            fail = false;

        // The following statements set machine constants used in various parts of the program.
        // The meaning of the four constants are:
        // eta     The maximum relative representation error which can be described as the
        //         smallest positive floating point number such that 1.0 + eta is greater than 1.
        // `are` and `mre` refer to the unit error in `+` and `*` respectively.  They are assumed
        // to be the same as eta.
        var eta = isc.Math._EPSILON,
            are = eta,
            mre = eta;

        // Initialization of constants for shift rotation.
        var xx = Math.SQRT1_2;
        var yy = -xx;
        var cosr = Math.cos(94 * Math.PI / 180);
        var sinr = Math.sin(94 * Math.PI / 180);

        // Skip any leading coefficients that are zero.
        var op0 = 0;
        while (op0 <= degree && op[op0] == 0) {
            ++op0;
        }
        degree -= Math.min(degree, op0);
        
        if (degree == 0) {
            // Fail if the degree is zero.  The polynomial is constant so there are either
            // zero roots or infinitely many roots.
            fail = true;
            degree = 0;
            return { degree: degree, zeros: zeros, fail: fail };
        }
        state.n = degree;
        state.nn = degree + 1;

        // Remove the zeros at the origin if any.
        while (op[op0 + state.nn - 1] == 0) {
            var j = degree - state.n;
            zeros[j] = new isc.Math._complexNumber(0, 0);
            --state.nn;
            --state.n;
        }

        // Make a copy of the coefficients.
        for (var i = state.nn; i--; ) {
            state.p[i] = op[op0 + i];
        }

        var pt = new Array(101),
            temp = new Array(101);
        for (;;) {
            // Start the algorithm for one zero.
            if (state.n <= 2) {
                // Calculate the final zero or pair of zeros.
                if (state.n == 2) {
                    var quadOutput = isc.Math._quad(state.p[0], state.p[1], state.p[2]);
                    zeros[degree - 2] = new isc.Math._complexNumber(quadOutput.sr, quadOutput.si);
                    zeros[degree - 1] = new isc.Math._complexNumber(quadOutput.lr, quadOutput.li);
                } else if (state.n == 1) {
                    zeros[degree - 1] = new isc.Math._complexNumber(-state.p[1] / state.p[0], 0);
                }
                return { degree: degree, zeros: zeros, fail: fail };
            }

            // Find largest and smallest moduli of coefficients.
            var max = 0;
            var min = Number.MAX_VALUE;
            for (var i = state.nn; i--; ) {
                var x = Math.abs(state.p[i]);
                if (x > max) {
                    max = x;
                }
                if (x != 0 && x < min) {
                    min = x;
                }
            }

            // Scale if there are large or very small coefficients computes a scale factor to multiply
            // the coefficients of the polynomial.  The scaling is done to avoid overflow and to
            // avoid undetected underflow interfering with the convergence criterion.  The factor is
            // a power of 10.
            var sc = (Number.MIN_VALUE / eta) / min;
            if ((sc > 1 && Number.MAX_VALUE / sc >= max) || max >= 10) {
                if (sc == 0) {
                    sc = Number.MIN_VALUE;
                }
                var factor = Math.pow(10, Math.floor(Math.log(sc) / Math.LN10 + 0.5));
                if (factor != 1) {
                    for (var i = state.nn; i--; ) {
                        state.p[i] *= factor;
                    }
                }
            }

            // Compute lower bound on moduli of zeros.
            for (var i = state.nn; i--; ) {
                pt[i] = Math.abs(state.p[i]);
            }
            pt[state.nn - 1] = -pt[state.nn - 1];
            // Compute upper estimate of bound.
            var x = Math.exp((Math.log(-pt[state.nn - 1]) - Math.log(pt[0])) / state.n);
            if (pt[state.n - 1] != 0) {
                // If Newton step at the origin is better, use it.
                var xm = -pt[state.nn - 1] / pt[state.n - 1];
                if (xm < x) {
                    x = xm;
                }
            }

            // Chop the interval (0,x) until ff <= 0.
            for (;;) {
                var xm = x / 10;
                var ff = pt[0];
                for (var i = 1; i < state.nn; ++i) {
                    ff = ff * xm + pt[i];
                }
                if (ff > 0) {
                    x = xm;
                } else {
                    break;
                }
            }

            // Do Newton iteration until x converges to two decimal places.
            for (var dx = x; Math.abs(dx / x) > 5e-3; ) {
                var ff = pt[0];
                var df = ff;
                for (var i = 1; i < state.n; ++i) {
                    ff = ff * x + pt[i];
                    df = df * x + ff;
                }
                ff = ff * x + pt[state.nn - 1];
                dx = ff / df;
                x -= dx;
            }

            var bnd = x;
            // Compute the derivative as the intial k polynomial and do 5 steps with no shift.
            var nm1 = state.n - 1;
            state.k[0] = state.p[0];
            for (var i = state.n; (--i) >= 1; ) {
                state.k[i] = (state.nn - i - 1) * state.p[i] / state.n;
            }
            var aa = state.p[state.nn - 1];
            var bb = state.p[state.n - 1];
            var zerok = (state.k[state.n - 1] == 0);
            for (var jj = 5; jj--; ) {
                var cc = state.k[state.n - 1];
                if (zerok) {
                    // Use unscaled form of recurrence.
                    for (var i = 0; i < nm1; ++i) {
                        var j = state.nn - i;
                        state.k[j] = state.k[j - 1];
                    }
                    state.k[0] = 0;
                    zerok = (state.k[state.n - 1] == 0);
                } else {
                    // Use scaled form of recurrence if value of k at 0 is nonzero.
                    var t = -aa / cc;
                    for (var i = 0; i < nm1; ++i) {
                        var j = state.nn - i;
                        state.k[j] = t * state.k[j - 1] + state.p[j];
                    }
                    state.k[0] = state.p[0];
                    zerok = (Math.abs(state.k[state.n - 1]) <= Math.abs(bb) * eta * 10);
                }
            }

            // Save k for restarts with new shifts.
            for (var i = state.n; i--; ) {
                temp[i] = state.k[i];
            }

            // Loop to select the quadratic corresponding to each new shift.
            for (var cnt = 1; cnt <= 20; ++cnt) {
                // Quadratic corresponds to a double shift to a non-real point and its complex
                // conjugate.  The point has modulus bnd and amplitude rotated by 94 degrees from
                // the previous shift.
                var xxx = cosr * xx - sinr * yy;
                yy = sinr * xx + cosr * yy;
                xx = xxx;
                state.sr = bnd * xx;
                state.si = bnd * yy;
                state.u = state.sr * -2;
                state.v = bnd;
                // Second stage calculation, fixed quadratic
                var nz = isc.Math._fxshfr(state, cnt * 20, eta, are, mre);
                if (nz != 0) {
                    // The second stage jumps directly to one of the third stage iterations and
                    // returns here if successful.  Deflate the polynomial, store the zero or
                    // zeros and return to the main algorithm.
                    var j = degree - state.n;
                    zeros[j] = new isc.Math._complexNumber(state.szr, state.szi);
                    state.nn -= nz;
                    state.n = state.nn - 1;
                    for (var i = state.nn; i--; ) {
                        state.p[i] = state.qp[i];
                    }
                    if (nz != 1) {
                        zeros[j + 1] = new isc.Math._complexNumber(state.lzr, state.lzi);
                    }
                    break;
                }

                // If the iteration is unsuccessful another quadratic is chosen after restoring k.
                for (var i = state.n; i--; ) {
                    state.k[i] = temp[i];
                }
            }
        }

        // Return with failure if no convergence with 20 shifts.
        fail = true;
        degree -= state.n;
        return { degree: degree, zeros: zeros, fail: fail };
    },

    
    _fxshfr : function (state, l2, eta, are, mre) {

        var betav = 0.25;
        var betas = 0.25;
        var oss = state.sr;
        var ovv = state.v;
        var otv = 0, ots = 0;

        // Evaluate polynomial by synthetic division.
        var quadsdOutput = isc.Math._quadsd(state.nn, state.u, state.v, state.p);
        for (var i = state.nn; i--; ) {
            state.qp[i] = quadsdOutput.q[i];
        }
        state.a = quadsdOutput.a;
        state.b = quadsdOutput.b;

        var type = isc.Math._calcsc(state, eta);
        var vv = 0, ss = 0, tv = 0, ts = 0;
        for (var j = 1; j <= l2; ++j, ovv = vv, oss = ss, otv = tv, ots = ts) {
            // Calculate next k polynomial and estimate v.
            isc.Math._nextk(state, type, eta);
            type = isc.Math._calcsc(state, eta);
            var newestOutput = isc.Math._newest(state, type);
            var ui = newestOutput.uu;
            var vi = newestOutput.vv;

            vv = vi;
            // Estimate s.
            ss = 0;
            if (state.k[state.n - 1] != 0) {
                ss = -state.p[state.nn - 1] / state.k[state.n - 1];
            }
            tv = 1;
            ts = 1;
            if (j == 1 || type == 3) {
                continue;
            }
            // Compute relative measures of convergence of s and v sequences.
            if (vv != 0) {
                tv = Math.abs((vv - ovv) / vv);
            }
            if (ss != 0) {
                ts = Math.abs((ss - oss) / ss);
            }
            // If decreasing, multiply two most recent convergence measures.
            var tvv = 1;
            if (tv < otv) {
                tvv = tv * otv;
            }
            var tss = 1;
            if (ts < ots) {
                tss = ts * ots;
            }
            // Compare with convergence criteria.
            var vpass = tvv < betav;
            var spass = tss < betas;
            if (!(spass || vpass)) {
                continue;
            }
            // At least one sequence has passed the convergence test.  Store variables before iterating.
            var svu = state.u;
            var svv = state.v;
            for (var i = state.n; i--; ) {
                state.svk[i] = state.k[i];
            }
            var s = ss;
            // Choose iteration according to the fastest converging sequence.
            var vtry = false;
            var stry = false;
            for (var tryQuadraticIteration = !(spass && (!vpass || tss < tvv)); true; ) {
                if (tryQuadraticIteration) {
                    var nz = isc.Math._quadit(state, ui, vi, eta, are, mre);
                    if (nz > 0) {
                        return nz;
                    }

                    // Quadratic iteration has failed. flag that it has been tried and decrease the convergence criterion.
                    vtry = true;
                    betav /= 4;
                    // Try linear iteration if it has not been tried and the s sequence is converging.
                    if (stry || !spass) {
                        // Restore variables.
                        state.u = svu;
                        state.v = svv;
                        for (var i = state.n; i--; ) {
                            state.k[i] = state.svk[i];
                        }
                        if (vpass && !vtry) {
                            tryQuadraticIteration = true;
                            continue;
                        } else {
                            break;
                        }
                    }
                    for (var i = state.n; i--; ) {
                        state.k[i] = state.svk[i];
                    }
                }

                var realitOutput = isc.Math._realit(state, s, eta, are, mre);
                s = realitOutput.sss;
                var nz = realitOutput.nz;
                var iflag = realitOutput.iflag;
                if (nz > 0) {
                    return nz;
                }

                // Linear iteration has failed.  Flag that it has been tried and decrease the convergence criterion.
                stry = true;
                betas /= 4;
                if (iflag != 0) {
                    // If linear iteration signals an almost double real zero attempt quadratic interation.
                    ui = -(s + s);
                    vi = s * s;
                    tryQuadraticIteration = true;
                    continue;
                }

                // Restore variables.
                state.u = svu;
                state.v = svv;
                for (var i = state.n; i--; ) {
                    state.k[i] = state.svk[i];
                }
                // Try quadratic iteration if it has not been tried and the v sequence is converging.
                tryQuadraticIteration = vpass && !vtry;
                if (!tryQuadraticIteration) {
                    break;
                }
            }

            // Recompute qp and scalar values to continue the second stage.
            var quadsdOutput = isc.Math._quadsd(state.nn, state.u, state.v, state.p);
            for (var i = state.nn; i--; ) {
                state.qp[i] = quadsdOutput.q[i];
            }
            state.a = quadsdOutput.a;
            state.b = quadsdOutput.b;

            type = isc.Math._calcsc(state, eta);
        }
        return 0;
    },

    
    _quadit : function (state, uu, vv, eta, are, mre) {

        state.u = uu;
        state.v = vv;

        var tried = false,
            relstp = 0,
            omp = 0;

        var j = 0;
        for (;;) {
            var quadOutput = isc.Math._quad(1, state.u, state.v);
            state.szr = quadOutput.sr;
            state.szi = quadOutput.si;
            state.lzr = quadOutput.lr;
            state.lzi = quadOutput.li;
            // Return if roots of the quadratic are real and not close to multiple or nearly equal
            // and of opposite sign.
            if (Math.abs(Math.abs(state.szr) - Math.abs(state.lzr))
                    > Math.abs(state.lzr) * 1e-2)
            {
                return 0;
            }
            // Evaluate polynomial by quadratic synthetic division.
            var quadsdOutput = isc.Math._quadsd(state.nn, state.u, state.v, state.p);
            for (var i = state.nn; i--; ) {
                state.qp[i] = quadsdOutput.q[i];
            }
            state.a = quadsdOutput.a;
            state.b = quadsdOutput.b;

            var mp = (
                Math.abs(state.a - state.szr * state.b) +
                Math.abs(state.szi * state.b));
            // Compute a rigorous bound on the rounding error in evaluting p.
            var zm = Math.sqrt(Math.abs(state.v));
            var ee = Math.abs(state.qp[0]) * 2;
            var t = -state.szr * state.b;
            for (var i = 1; i < state.n; ++i) {
                ee = ee * zm + Math.abs(state.qp[i]);
            }
            ee = ee * zm + Math.abs(state.a + t);
            ee = (
                (5 * mre + 4 * are) * ee -
                (5 * mre + 2 * are) * (Math.abs(state.a + t) + Math.abs(state.b) * zm) +
                are * 2 * Math.abs(t));
            // Iteration has converged sufficiently if the polynomial value is less than 20 times
            // this bound.
            if (mp <= ee * 20) {
                return 2;
            }

            ++j;
            // Stop iteration after 20 steps.
            if (j > 20) {
                return 0;
            }
            if (!(j < 2 || relstp > 1e-2 || mp < omp || tried)) {
                // A cluster appears to be stalling the convergence.  Five fixed shift steps are
                // taken with a u,v close to the cluster.
                relstp = Math.sqrt(Math.max(eta, relstp));
                state.u -= state.u * relstp;
                state.v += state.v * relstp;
                var quadsdOutput = isc.Math._quadsd(state.nn, state.u, state.v, state.p);
                for (var i = state.nn; i--; ) {
                    state.qp[i] = quadsdOutput.q[i];
                }
                state.a = quadsdOutput.a;
                state.b = quadsdOutput.b;

                for (var i = 5; i--; ) {
                    isc.Math._nextk(state, isc.Math._calcsc(state, eta), eta);
                }
                tried = true;
                j = 0;
            }

            omp = mp;
            // Calculate next k polynomial and new u and v.
            isc.Math._nextk(state, isc.Math._calcsc(state, eta), eta);
            var newestOutput = isc.Math._newest(state, isc.Math._calcsc(state, eta));
            var ui = newestOutput.uu;
            var vi = newestOutput.vv;

            // If vi is zero the iteration is not converging.
            if (vi == 0) {
                return 0;
            }
            relstp = Math.abs((vi - state.v) / vi);
            state.u = ui;
            state.v = vi;
        }
    },

    
    _realit : function (state, sss, eta, are, mre) {

        var nm1 = state.n - 1;

        var s = sss;
        var nz = 0;
        var iflag = 0;

        for (var j = 0, omp = 0; true; ) {
            var pv = state.p[0];
            // Evaluate p at s.
            state.qp[0] = pv;
            for (var i = 1; i < state.nn; ++i) {
                pv = pv * s + state.p[i];
                state.qp[i] = pv;
            }
            var mp = Math.abs(pv);
            // Compute a rigorous bound on the error in evaluating p.
            var ms = Math.abs(s);
            var ee = mre / (are + mre) * Math.abs(state.qp[0]);
            for (var i = 1; i < state.nn; ++i) {
                ee = ee * ms + Math.abs(state.qp[i]);
            }
            // Iteration has converged sufficiently if the polynomial value is less than 20 times
            // this bound.
            if (mp <= ((are + mre) * ee - mre * mp) * 20) {
                nz = 1;
                state.szr = s;
                state.szi = 0;
                return { sss: sss, nz: nz, iflag: iflag };
            }

            ++j;
            // Stop iteration after 10 steps.
            if (j > 10) {
                return { sss: sss, nz: nz, iflag: iflag };
            }
            if (!(j < 2 || Math.abs(t) > Math.abs(s - t) * 1e-3 || mp <= omp)) {
                // A cluster of zeros near the real axis has been encountered.  Return with `iflag`
                // set to initiate a quadratic iteration.
                iflag = 1;
                sss = s;
                return { sss: sss, nz: nz, iflag: iflag };
            }

            // Return if the polynomial value has increased significantly.
            omp = mp;
            // Compute `t`, the next polynomial, and the new iterate.
            var kv = state.k[0];
            state.qk[0] = kv;
            for (var i = 1; i < state.n; ++i) {
                kv = kv * s + state.k[i];
                state.qk[i] = kv;
            }
            if (Math.abs(kv) <= Math.abs(state.k[state.n - 1]) * 10 * eta) {
                // Use unscaled form.
                state.k[0] = 0;
                for (var i = state.n; (--i) >= 1; ) {
                    state.k[i] = state.qk[i - 1];
                }
            } else {
                // Use the scaled form of the recurrence if the value of k at s is nonzero.
                var t = -pv / kv;
                state.k[0] = state.qp[0];
                for (var i = state.n; (--i) >= 1; ) {
                    state.k[i] = t * state.qk[i - 1] + state.qp[i];
                }
            }

            kv = state.k[0];
            for (var i = 1; i < state.n; ++i) {
                kv = kv * s + state.k[i];
            }
            var t = 0;
            if (Math.abs(kv) > Math.abs(state.k[state.n - 1]) * 10 * eta) {
                t = -pv / kv;
            }
            s += t;
        }
    },

    
    _calcsc : function (state, eta) {
        // Synthetic division of `k` by the quadratic `1,u,v`.
        var quadsdOutput = isc.Math._quadsd(state.n, state.u, state.v, state.k);
        for (var i = state.n; i--; ) {
            state.qk[i] = quadsdOutput.q[i];
        }
        state.c = quadsdOutput.a;
        state.d = quadsdOutput.b;

        var isType3 = !(
            Math.abs(state.c) > Math.abs(state.k[state.n - 1]) * 100 * eta ||
            Math.abs(state.d) > Math.abs(state.k[state.n - 2]) * 100 * eta);

        if (isType3) {
            // type=3 indicates the quadratic is almost a factor of k.
            return 3;
        } else if (Math.abs(state.d) < Math.abs(state.c)) {
            // type=1 indicates that all formulas are divided by c.
            state.e = state.a / state.c;
            state.f = state.d / state.c;
            state.g = state.u * state.e;
            state.h = state.v * state.b;
            state.a3 = state.a * state.e + (state.h / state.c + state.g) * state.b;
            state.a1 = state.b - state.a * (state.d / state.c);
            state.a7 = state.a + state.g * state.d + state.h * state.f;
            return 1;
        } else {
            // type=2 indicates that all formulas are divided by d.
            state.e = state.a / state.d;
            state.f = state.c / state.d;
            state.g = state.u * state.b;
            state.h = state.v * state.b;
            state.a3 = (state.a + state.g) * state.e + state.h * (state.b / state.d);
            state.a1 = state.b * state.f - state.a;
            state.a7 = (state.f + state.u) * state.a + state.h;
            return 2;
        }
    },

    
    _nextk : function (state, type, eta) {
        if (type == 3) {
            // Use unscaled form of the recurrence if type is 3.
            state.k[0] = state.k[1] = 0;
            for (var i = state.n; (--i) >= 2; ) {
                state.k[i] = state.qk[i - 2];
            }
        } else {
            var temp = (type == 1 ? state.b : state.a);
            if (Math.abs(state.a1) > Math.abs(temp) * eta * 10) {
                // Use scaled form of the recurrence.
                state.a7 /= state.a1;
                state.a3 /= state.a1;
                state.k[0] = state.qp[0];
                state.k[1] = state.qp[1] - state.a7 * state.qp[0];
                for (var i = state.n; (--i) >= 2; ) {
                    state.k[i] = (
                        state.a3 * state.qk[i - 2] - state.a7 * state.qp[i - 1] + state.qp[i]);
                }
            } else {
                // If a1 is nearly zero then use a special form of the recurrence.
                state.k[0] = 0;
                state.k[1] = -state.a7 * state.qp[0];
                for (var i = state.n; (--i) >= 2; ) {
                    state.k[i] = state.a3 * state.qk[i - 2] - state.a7 * state.qp[i - 1];
                }
            }
        }
    },

    
    _newest : function (state, type) {
        if (type != 3) {
            var a4, a5;
            if (type == 2) {
                a4 = (state.a + state.g) * state.f + state.h;
                a5 = (state.f + state.u) * state.c + state.v * state.d;
            } else {
                a4 = state.a + state.u * state.b + state.h * state.f;
                a5 = state.c + (state.u + state.v * state.f) * state.d;
            }
            // Evaluate new quadratic coefficients.
            var b1 = -state.k[state.n - 1] / state.p[state.nn - 1];
            var b2 = -(state.k[state.n - 2] + b1 * state.p[state.n - 1]) / state.p[state.nn - 1];
            var c1 = state.v * b2 * state.a1;
            var c2 = b1 * state.a7;
            var c3 = b1 * b1 * state.a3;
            var c4 = c1 - c2 - c3;
            var temp = a5 + b1 * a4 - c4;
            if (temp != 0) {
                var uu = state.u - (state.u * (c3 + c2) + state.v * 
                                    (b1 * state.a1 + b2 * state.a7)) / temp;
                var vv = state.v * (c4 / temp + 1);
                return { uu: uu, vv: vv };
            }
        }
        // If type=3 the quadratic is zeroed.
        return { uu: 0, vv: 0 };
    },

    
    _quadsd : function (nn, u, v, p) {
        var q = new Array(nn),
            b = q[0] = p[0],
            a = q[1] = p[1] - u * b;
        for (var i = 2; i < nn; ++i) {
            var c = p[i] - u * a - v * b;
            q[i] = c;
            b = a;
            a = c;
        }

        return { q: q, a: a, b: b };
    },

    
    _quad : function (a, b1, c) {

        var sr = 0, si = 0, lr = 0, li = 0;

        if (a == 0) {
            sr = 0;
            if (b1 != 0) {
                sr = -c / b1;
            }
            lr = 0;

            si = 0;
            li = 0;
            return { sr: sr, si: si, lr: lr, li: li };
        } else if (c == 0) {
            sr = 0;
            lr = -b1 / a;

            si = 0;
            li = 0;
            return { sr: sr, si: si, lr: lr, li: li };
        }

        // Compute discriminant avoiding overflow.
        var b = b1 / 2, d = 0, e = 0;
        if (Math.abs(b) < Math.abs(c)) {
            e = a;
            if (c < 0) {
                e = -a;
            }
            e = b * (b / Math.abs(c)) - e;
            d = Math.sqrt(Math.abs(e)) * Math.sqrt(Math.abs(c));
        } else {
            e = 1 - a / b * (c / b);
            d = Math.sqrt(Math.abs(e)) * Math.abs(b);
        }
        if (e < 0) {
            // complex conjugate zeros
            sr = -b / a;
            lr = sr;
            si = Math.abs(d / a);
            li = -si;
            return { sr: sr, si: si, lr: lr, li: li };
        } else {
            // real zeros
            if (b >= 0) {
                d = -d;
            }
            lr = (-b + d) / a;
            sr = 0;
            if (lr != 0) {
                sr = c / lr / a;
            }
            si = 0;
            li = 0;
            return { sr: sr, si: si, lr: lr, li: li };
        }
    },

    _gcd : function (a, b) {
        
        if (a < 0) {
            a = -a;
        }
        if (b < 0) {
            b = -b;
        }
        if (a == 0) {
            return b;
        } else if (b == 0) {
            return a;
        } else {
            var r0 = a, r1 = b, r2 = 0;
            while ((r2 = r0 % r1) != 0) {
                r0 = r1;
                r1 = r2;
            }
            return r1;
        }
    },

    _isAnAffineTransformDecomposition : function (obj) {
        return (
            isc.isAn.Object(obj) &&
            isc.isAn.Array(obj.translate) && obj.translate.length == 2 &&
            isc.isA.Number(obj.translate[0]) && isc.isA.Number(obj.translate[1]) &&
            isc.isAn.Array(obj.scale) && obj.scale.length == 2 &&
            isc.isA.Number(obj.scale[0]) && isc.isA.Number(obj.scale[1]) &&
            isc.isA.Number(obj.xShearFactor) &&
            isc.isA.Number(obj.yShearFactor) &&
            isc.isA.Number(obj.rotation) &&
            isc.isAn.Array(obj.rotationCenter) && obj.rotationCenter.length == 2 &&
            isc.isA.Number(obj.rotationCenter[0]) && isc.isA.Number(obj.rotationCenter[1]));
    },

    // A constructor function for AffineTransformDecomposition objects.
    _affineTransformDecomposition : function (
            translate, scale, xShearFactor, yShearFactor, rotation, rotationCenter)
    {
        this.translate = translate;
        this.scale = scale;
        this.xShearFactor = xShearFactor;
        this.yShearFactor = yShearFactor;
        this.rotation = rotation;
        this.rotationCenter = rotationCenter;
        
    }
};


isc.defineClass("AffineTransform").addClassProperties({
    // Rotation by an angle about a given point (cx, cy) is equivalent to:
    // 1. Translating by -cx, -cy. (A)
    // 2. Rotating by the angle.   (B)
    // 3. Translating by cx, cy.   (C)
    //
    // (*** C ****)(******** B *********)(*** A *****)
    // [1 , 0 , cx][cos(a) , -sin(a) , 0][1 , 0 , -cx]   [cos(a) , -sin(a) , cx][1 , 0 , -cx]   [cos(a) , -sin(a) , -cos(a) * cx + sin(a) * cy + cx]
    // [0 , 1 , cy][sin(a) ,  cos(a) , 0][0 , 1 , -cy] = [sin(a) ,  cos(a) , cy][0 , 1 , -cy] = [sin(a) ,  cos(a) , -sin(a) * cx - cos(a) * cy + cy]
    // [0 , 0 ,  1][     0 ,       0 , 1][0 , 0 ,   1]   [     0 ,       0 ,  1][0 , 0 ,   1]   [     0 ,       0 ,                               1]
    getRotateTransform : function (angle, cx, cy) {
        var c = isc.Math.cosdeg(angle),
            s = isc.Math.sindeg(angle);
        return isc.AffineTransform.create(
            c, -s, -c * cx + s * cy + cx,
            s, c, -s * cx - c * cy + cy);
    },

    getTranslateTransform : function (dx, dy) {
        return isc.AffineTransform.create(1, 0, dx, 0, 1, dy);
    },

    // An internal singleton used to avoid wasting memory storing multiple copies of a trivial
    // transform:
    _identityTransform: isc.AffineTransform.create(),
    _getIdentityTransform : function () {
        return isc.AffineTransform._identityTransform;
    },

    
    _transformDecomposition: {
        dx: 0, dy: 0, sx: 0, sy: 0, kx: 0, ky: 0, theta: 0, cx: 0, cy: 0,
        h00: 0, h01: 0, h02: 0, h10: 0, h11: 0, h12: 0
    },
    _decomposeTransform : function (transform, cx, cy) {
        var m00 = transform.m00,
            m01 = transform.m01,
            m02 = transform.m02,
            m10 = transform.m10,
            m11 = transform.m11,
            m12 = transform.m12,
            output = isc.AffineTransform._transformDecomposition;

        if (m00 == 0 && m01 == 0 && m10 == 0 && m11 == 0) {
            output.sx = output.sy = output.kx = output.ky = output.theta = 0;
            output.dx = output.h02 = m02;
            output.dy = output.h12 = m12;
            output.h00 = output.h01 = output.h10 = output.h11;
            return output;
        }

        var epsilon = 1e-9,
            det = m00 * m11 - m01 * m10,
            singular = (Math.abs(det) < epsilon);

        var absDet = Math.abs(det),
            signDet = (det < 0 ? -1 : 1),
            u00 = m00 + signDet * m11,
            u01 = m01 - signDet * m10,
            u10 = m10 - signDet * m01,
            u11 = m11 + signDet * m00,
            detU = u00 * u11 - u01 * u10,
            gamma = Math.sqrt(Math.abs(detU));
        
        var reflected = (detU < 0),
            theta = Math.atan2(u10, u00);
        u00 /= gamma;
        u01 /= gamma;
        u10 /= gamma;
        u11 /= gamma;

        

        // H = (A^t * A + |det A| * I) / gamma
        var h00 = (m00 * m00 + m10 * m10 + absDet) / gamma,
            h01 = (m00 * m01 + m10 * m11) / gamma,
            h10 = h01,
            h11 = (m01 * m01 + m11 * m11 + absDet) / gamma;

        // S = U * H * U^-1
        var c = Math.cos(theta),
            s = Math.sin(theta),
            c2 = c * c,
            s2 = s * s,
            // c^2 - s^2 = cos(2 * theta)
            c2ms2 = Math.cos(2 * theta),
            // 2 * c * s = sin(2 * theta)
            twocs = Math.sin(2 * theta),
            s00 = 0, s01 = 0, s10 = 0, s11 = 0;
        if (reflected) {
            s00 = c2 * h00 + twocs * h01 + s2 * h11;
            s01 = -c2ms2 * h01 + c * s * (h00 - h11);
            s10 = s01;
            s11 = c2 * h11 - twocs * h01 + s2 * h00;
        } else {
            s00 = c2 * h00 - twocs * h01 + s2 * h11;
            s01 = c2ms2 * h01 + c * s * (h00 - h11);
            s10 = s01;
            s11 = c2 * h11 + twocs * h01 + s2 * h00;
        }
        var detS = (s00 * s11 - s01 * s10);

        

        // Calculate parameters sx, sy, kx, ky, dx, and dy.
        var sx = 0, sy = 0, kx = 0, ky = 0, dx = 0, dy = 0;
        if (reflected) {
            

            // alpha = s * (2 * c - 1) = 2 * c * s - s
            var alpha = twocs - s,
                // beta = ((c + 1) * s^2 + (c - 1) * c^2)
                //      = c * (s^2 + c^2) + (s^2 - c^2)
                //      = c - (c^2 - s^2)
                beta = c - c2ms2,
                gamma = (-cx * beta + cy * alpha),
                delta = (cx * alpha + cy * beta);

            dx = m02 - (s00 * gamma + s01 * delta);
            dy = m12 - (s01 * gamma + s11 * delta);

            sy = twocs * s01 - c2ms2 * s11;
            
            ky = (c2ms2 * s01 + twocs * s11) / sy;
            sx = -detS / sy;
            kx = (twocs * s00 - c2ms2 * s01) / sx;

        } else {
            var alpha = (cy * s + cx * (1 - c)),
                beta = (-cx * s + cy * (1 - c));
            dx = m02 - (alpha * s00 + beta * s01);
            dy = m12 - (alpha * s01 + beta * s11);

            if (s00 == 0 || s11 == 0 || s01 == 0) {
                
                sx = s00;
                sy = s11;
                kx = ky = 0;
            } else if (detS == 0) {
                
                sx = s00;
                sy = s11;
                kx = ky = 0;
            } else {
                // `s00`, `s11`, and `detS` are all greater than zero.
                sx = detS / s11;
                sy = s11;
                kx = s01 / sx;
                ky = s01 / sy;
            }
        }

        

        output.dx = dx;
        output.dy = dy;
        output.sx = sx;
        output.sy = sy;
        output.kx = kx;
        output.ky = ky;
        output.theta = theta;
        output.cx = cx;
        output.cy = cy;
        output.h00 = h00;
        output.h01 = h01;
        output.h10 = (reflected ? -h10 : h10);
        output.h11 = (reflected ? -h11 : h11);
        output.h02 = m02 * c + m12 * s;
        output.h12 = -m02 * s + m12 * c;
        
        return output;
    },

    recompose : function (decomp, output) {
        
        if (output == null) {
            output = isc.AffineTransform.create();
        }
        if (decomp == null) {
            return;
        }

        var dx = 0, dy = 0, sx = 1, sy = 1, kx = 0, ky = 0, theta = 0, cx = 0, cy = 0;
        if (isc.isAn.Array(decomp.translate)) {
            if (isc.isA.Number(decomp.translate[0])) {
                dx = decomp.translate[0];
            }
            if (isc.isA.Number(decomp.translate[1])) {
                dy = decomp.translate[1];
            }
        }
        if (isc.isAn.Array(decomp.scale)) {
            if (isc.isA.Number(decomp.scale[0])) {
                sx = decomp.scale[0];
            }
            if (isc.isA.Number(decomp.scale[1])) {
                sy = decomp.scale[1];
            }
        }
        if (isc.isA.Number(decomp.xShearFactor)) {
            kx = decomp.xShearFactor;
        }
        if (isc.isA.Number(decomp.yShearFactor)) {
            ky = decomp.yShearFactor;
        }
        if (isc.isA.Number(decomp.rotation)) {
            theta = decomp.rotation * Math.PI / 180;
        }
        if (isc.isAn.Array(decomp.rotationCenter)) {
            if (isc.isA.Number(decomp.rotationCenter[0])) {
                cx = decomp.rotationCenter[0];
            }
            if (isc.isA.Number(decomp.rotationCenter[1])) {
                cy = decomp.rotationCenter[1];
            }
        }

        var sin = Math.sin(theta), cos = Math.cos(theta),
            sxsin = sx * sin, sxcos = sx * cos,
            sysin = sy * sin, sycos = sy * cos,
            kxkyp1 = kx * ky + 1,
            kycymcx = ky * cy - cx,
            kycxpcy = ky * cx + cy;

        output.setTransform(
            kx * sxsin + kxkyp1 * sxcos,
            -kxkyp1 * sxsin + kx * sxcos,
            (kx * kycymcx + cy) * sxsin + (kx * kycxpcy + cx) * sx * (1 - cos) + dx,
            sysin + ky * sycos,
            sycos - ky * sysin,
            kycymcx * sysin + kycxpcy * sy * (1 - cos) + dy);
        return output;
    }
});

isc.AffineTransform.addProperties({
    // Start with the identity transform.
    m00: 1, m01: 0, m02: 0,
    m10: 0, m11: 1, m12: 0,

    addPropertiesOnCreate: false,
    init : function (m00, m01, m02, m10, m11, m12) {
        if (
            isc.isA.Number(m00) && isc.isA.Number(m01) && isc.isA.Number(m02) &&
            isc.isA.Number(m10) && isc.isA.Number(m11) && isc.isA.Number(m12))
        {
            this.m00 = m00;
            this.m01 = m01;
            this.m02 = m02;
            this.m10 = m10;
            this.m11 = m11;
            this.m12 = m12;
        } else if (isc.Math._isAnAffineTransformDecomposition(m00)) {
            return isc.AffineTransform.recompose(m00);
        } else {
            var numArgs = arguments.length;
            for (var i = 0; i < numArgs; ++i) {
                var arg = arguments[i];
                if (isc.isAn.Object(arg)) {
                    isc.addProperties(this, arg);
                }
            }
        }
        return this.Super("init", arguments);
    },

    duplicate : function () {
        return isc.AffineTransform.create(
            this.m00, this.m01, this.m02, this.m10, this.m11, this.m12);
    },

    _copy : function (output) {
        
        output.setTransform(this.m00, this.m01, this.m02, this.m10, this.m11, this.m12);
        return output;
    },

    getDeterminant : function () {
        return this.m00 * this.m11 - this.m10 * this.m01;
    },

    getInverse : function (output) {
        
        var det = this.getDeterminant(),
            isInvertible = isc.isA.Number(det) && det != 0;
        
        if (!isInvertible) return null;

        if (output == null) {
            output = isc.AffineTransform.create();
        }
        output.setTransform(
            this.m11 / det,
            -this.m01 / det,
            (this.m01 * this.m12 - this.m11 * this.m02) / det,
            -this.m10 / det,
            this.m00 / det,
            (this.m10 * this.m02 - this.m00 * this.m12) / det);
        return output;
    },

    // [t00 , t01 , t02][m00 , m01 , m02]   [t00 * m00 + t01 * m10 , t00 * m01 + t01 * m11 , t00 * m02 + t01 * m12 + t02]
    // [t10 , t11 , t12][m10 , m11 , m12] = [t10 * m00 + t11 * m10 , t10 * m01 + t11 * m11 , t10 * m02 + t11 * m12 + t12]
    // [  0 ,   0 ,   1][  0 ,   0 ,   1]   [                    0 ,                     0 ,                           1]
    leftMultiply : function (transform) {
        
        var m0 = this.m00,
            m1 = this.m10;
        this.m00 = transform.m00 * m0 + transform.m01 * m1;
        this.m10 = transform.m10 * m0 + transform.m11 * m1;

        m0 = this.m01;
        m1 = this.m11;
        this.m01 = transform.m00 * m0 + transform.m01 * m1;
        this.m11 = transform.m10 * m0 + transform.m11 * m1;

        m0 = this.m02;
        m1 = this.m12;
        this.m02 = transform.m00 * m0 + transform.m01 * m1 + transform.m02;
        this.m12 = transform.m10 * m0 + transform.m11 * m1 + transform.m12;
        return this;
    },

    // [m00 , m01 , m02][t00 , t01 , t02]   [m00 * t00 + m01 * t10 , m00 * t01 + m01 * t11 , m00 * t02 + m01 * t12 + m02]
    // [m10 , m11 , m12][t10 , t11 , t12] = [m10 * t00 + m11 * t10 , m10 * t01 + m11 * t11 , m10 * t02 + m11 * t12 + m12]
    // [  0 ,   0 ,   1][  0 ,   0 ,   1]   [                    0 ,                     0 ,                           1]
    rightMultiply : function (transform) {
        
        var mx = this.m00,
            my = this.m01;
        this.m00 = mx * transform.m00 + my * transform.m10;
        this.m01 = mx * transform.m01 + my * transform.m11;
        this.m02 = mx * transform.m02 + my * transform.m12 + this.m02;

        mx = this.m10;
        my = this.m11;
        this.m10 = mx * transform.m00 + my * transform.m10;
        this.m11 = mx * transform.m01 + my * transform.m11;
        this.m12 = mx * transform.m02 + my * transform.m12 + this.m12;
        return this;
    },

    preRotate : function (angle, cx, cy) {
        return this.leftMultiply(isc.AffineTransform.getRotateTransform(angle, cx, cy));
    },

    //> affineTransform.rotate()
    // Adds a rotation transform to this affine transform.
    //
    // @param angle (double) the angle in degrees.
    // @param cx (double) X coordinate of the center of rotation.
    // @param cy (double) Y coordinate of the center of rotation.
    //<
    rotate : function (angle, cx, cy) {
        return this.rightMultiply(isc.AffineTransform.getRotateTransform(angle, cx, cy));
    },

    // [sx ,  0 , 0][m00 , m01 , m02]   [sx * m00 , sx * m01 , sx * m02]
    // [ 0 , sy , 0][m10 , m11 , m12] = [sy * m10 , sy * m11 , sy * m12]
    // [ 0 ,  0 , 1][  0 ,   0 ,   1]   [       0 ,        0 ,        1]
    preScale : function (sx, sy) {
        
        this.m00 *= sx; this.m01 *= sx; this.m02 *= sx;
        this.m10 *= sy; this.m11 *= sy; this.m12 *= sy;
        return this;
    },

    // [m00 , m01 , m02][sx ,  0 , 0]   [m00 * sx , m01 * sy , m02]
    // [m10 , m11 , m12][ 0 , sy , 0] = [m10 * sx , m11 * sy , m12]
    // [  0 ,   0 ,   1][ 0 ,  0 , 1]   [       0 ,        0 ,   1]
    scale : function (sx, sy) {
        
        this.m00 *= sx; this.m01 *= sy;
        this.m10 *= sx; this.m11 *= sy;
        return this;
    },

    // [1 , 0 , dx][m00 , m01 , m02]   [m00 , m01 , m02 + dx]
    // [0 , 1 , dy][m10 , m11 , m12] = [m10 , m11 , m12 + dy]
    // [0 , 0 ,  1][  0 ,   0 ,   1]   [  0 ,   0 ,        1]
    preTranslate : function (dx, dy) {
        
        this.m02 += dx;
        this.m12 += dy;
        return this;
    },

    // [m00 , m01 , m02][1 , 0 , dx]   [m00 , m01 , m00 * dx + m01 * dy + m02]
    // [m10 , m11 , m12][0 , 1 , dy] = [m10 , m11 , m10 * dx + m11 * dy + m12]
    // [  0 ,   0 ,   1][0 , 0 ,  1]   [  0 ,   0 ,                         1]
    translate : function (dx, dy) {
        
        this.m02 += this.m00 * dx + this.m01 * dy;
        this.m12 += this.m10 * dx + this.m11 * dy;
        return this;
    },

    // [1 , k , 0][m00 , m01 , m02]   [k * m10 + m00 , k * m11 + m01 , k * m12 + m02]
    // [0 , 1 , 0][m10 , m11 , m12] = [          m10 ,           m11 ,           m12]
    // [0 , 0 , 1][  0 ,   0 ,   1]   [            0 ,             0 ,             1]
    preXShear : function (kx) {
        
        this.m00 += kx * this.m10;
        this.m01 += kx * this.m11;
        this.m02 += kx * this.m12;
        return this;
    },

    // [m00 , m01 , m02][1 , k , 0]   [m00 , m01 + k * m00 , m02]
    // [m10 , m11 , m12][0 , 1 , 0] = [m10 , m11 + k * m10 , m12]
    // [  0 ,   0 ,   1][0 , 0 , 1]   [  0 ,             0 ,   1]
    xShear : function (kx) {
        
        this.m01 += kx * this.m00;
        this.m11 += kx * this.m10;
        return this;
    },

    // [1  , 0 , 0][m00 , m01 , m02]   [m00            , m01            , m02           ]
    // [ky , 1 , 0][m10 , m11 , m12] = [ky * m00 + m10 , ky * m01 + m11 , ky * m02 + m12]
    // [0  , 0 , 1][  0 ,   0 ,   1]   [             0 ,              0 ,              1]
    preYShear : function (ky) {
        
        this.m10 += ky * this.m00;
        this.m11 += ky * this.m01;
        this.m12 += ky * this.m02;
        return this;
    },

    // [m00 , m01 , m02][1  , 0 , 0]   [ky * m01 + m00 , m01 , m02]
    // [m10 , m11 , m12][ky , 1 , 0] = [ky * m11 + m10 , m11 , m12]
    // [  0 ,   0 ,   1][0  ,   , 1]   [             0 ,   0 ,   1]
    yShear : function (ky) {
        
        this.m00 += ky * this.m01;
        this.m10 += ky * this.m11;
        return this;
    },

    // [m00 , m01 , m02][v0]   [m00 * v0 + m01 * v1 + m02]
    // [m10 , m11 , m12][v1] = [m10 * v0 + m11 * v1 + m12]
    // [  0 ,   0 ,   1][ 1]   [                        1]
    transform : function (v0, v1, output) {
        if (isc.isAn.Array(v0)) {
            output = v1; v1 = v0[1]; v0 = v0[0];
        }
        if (!isc.isAn.Array(output)) {
            output = new Array(2);
        }
        output[0] = this.m00 * v0 + this.m01 * v1 + this.m02;
        output[1] = this.m10 * v0 + this.m11 * v1 + this.m12;
        return output;
    },

    setTransform : function (m00, m01, m02, m10, m11, m12) {
        this.m00 = m00;
        this.m01 = m01;
        this.m02 = m02;
        this.m10 = m10;
        this.m11 = m11;
        this.m12 = m12;
    },

    decompose : function (cx, cy) {
        if (isc.isAn.Array(cx)) {
            cy = cx[1]; cx = cx[0];
        }
        if (!(isc.isA.Number(cx) && isc.isA.Number(cy))) {
            cx = cy = 0;
        }
        var decomp = isc.AffineTransform._decomposeTransform(this, cx, cy);
        return new isc.Math._affineTransformDecomposition(
            [decomp.dx, decomp.dy], [decomp.sx, decomp.sy], decomp.kx, decomp.ky,
            decomp.theta * 180 / Math.PI, [cx, cy]);
    }
});
// NOTE: toString functions CANNOT be added by addMethods, because a property named "toString"
// will not be enumerated by for..in.  This is actually part of the ECMAScript standard!

isc.AffineTransform.getPrototype().toString = function () {
    return "[" + this.m00 + " , " + this.m01 + " , " + this.m02 + "]\n" +
           "[" + this.m10 + " , " + this.m11 + " , " + this.m12 + "]";
};
    
