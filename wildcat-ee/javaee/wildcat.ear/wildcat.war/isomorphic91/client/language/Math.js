/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
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
        return isc.AffineTransform.create({
            m00: c, m01: -s, m02: -c * cx + s * cy + cx,
            m10: s, m11:  c, m12: -s * cx - c * cy + cy
        });
    },

    getTranslateTransform : function (dx, dy) {
        return isc.AffineTransform.create({
            m00: 1, m01: 0, m02: dx,
            m10: 0, m11: 1, m12: dy
        });
    }
});

isc.AffineTransform.addProperties({
    // Start with the identity transform.
    m00: 1, m01: 0, m02: 0,
    m10: 0, m11: 1, m12: 0,

    duplicate : function () {
        return isc.AffineTransform.create({
            m00: this.m00, m01: this.m01, m02: this.m02,
            m10: this.m10, m11: this.m11, m12: this.m12
        });
    },

    getDeterminant : function () {
        return this.m00 * this.m11 - this.m10 * this.m01;
    },

    getInverse : function () {
        var det = this.getDeterminant(),
            isInvertible = isc.isA.Number(det) && det != 0;
        
        if (!isInvertible) return null;

        return isc.AffineTransform.create({
            m00: this.m11 / det,
            m10: -this.m10 / det,
            m01: -this.m01 / det,
            m11: this.m00 / det,
            m02: (this.m01 * this.m12 - this.m11 * this.m02) / det,
            m12: (this.m10 * this.m02 - this.m00 * this.m12) / det
        });
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

    // [m00 , m01 , m02][v0]   [m00 * v0 + m01 * v1 + m02]
    // [m10 , m11 , m12][v1] = [m10 * v0 + m11 * v1 + m12]
    // [  0 ,   0 ,   1][ 1]   [                        1]
    transform : function (v0, v1) {
        return {
            v0: this.m00 * v0 + this.m01 * v1 + this.m02,
            v1: this.m10 * v0 + this.m11 * v1 + this.m12
        };
    },

    toString : function () {
        return "[" + this.m00 + " , " + this.m01 + " , " + this.m02 + "]\n" +
               "[" + this.m10 + " , " + this.m11 + " , " + this.m12 + "]";
    }
});
