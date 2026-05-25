/**
 * Implementation classes for the {@link org.skyve.domain.number.NumberGenerator} SPI.
 *
 * <p>This package provides the persistence-backed number generation implementation used
 * at runtime:
 * <ul>
 *   <li>{@link org.skyve.impl.domain.number.AbstractDocumentNumberGenerator} — abstract
 *       base that implements the core algorithm: queries the {@code DocumentNumber} admin
 *       document to retrieve and atomically increment the next value for a given
 *       {@code (moduleName, documentName, fieldName)} key. Bootstraps from the maximum
 *       existing column value on first use.
 *   <li>{@link org.skyve.impl.domain.number.DocumentNumberGenerator} — concrete subclass
 *       that obtains the current thread's {@link org.skyve.persistence.Persistence}
 *       instance from {@link org.skyve.CORE#getPersistence()} and delegates to the
 *       abstract base.
 *   <li>{@link org.skyve.impl.domain.number.NumberGeneratorStaticSingleton} — startup
 *       holder for the active {@code NumberGenerator}; set once during application
 *       bootstrap and never changed thereafter.
 * </ul>
 *
 * <p>The singleton is accessed via {@link org.skyve.EXT#getNumberGenerator()}; callers
 * should never reference classes in this package directly.
 *
 * @see org.skyve.domain.number.NumberGenerator
 * @see org.skyve.EXT#getNumberGenerator()
 */
package org.skyve.impl.domain.number;
