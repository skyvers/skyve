package org.skyve.util;

/**
 * Numeric helper utilities.
 * <p>
 * Extracted from legacy {@code ModulesUtil}. Provides deterministic spreading
 * of incrementing integers over a large prime space using quadratic residue.
 */
public final class MathUtil {

    private MathUtil() { /* no instances */ }

    // Quadratic residue constants (from legacy ModulesUtil)
    private static final long PRIME = 4294967291L; // 4,294,967,291 (largest 32-bit prime)
    private static final long HALF_PRIME = PRIME / 2L;

    /**
     * Taking in an incrementing integer this function will create a fairly uniformly distributed,
     * sparse and unique set of numbers for inputs less than the prime (4,294,967,291).
     * See https://en.wikipedia.org/wiki/Quadratic_residue
     *
     * @param incrementingNumber The number to generate a unique pseudo random number for
     * @return The quadratic residue.
     */
    public static long getUniqueQuadraticResidue(long incrementingNumber) {
        long x = incrementingNumber + 1001; // for sufficient entropy
        long residue = (x * x) % PRIME;
        return (x <= HALF_PRIME) ? residue : (PRIME - residue);
    }
}
