/**
 * JAXB type adapters and mappers for Skyve domain types.
 *
 * <p>JAXB cannot natively serialize Skyve's temporal and numeric domain types
 * ({@link org.skyve.domain.types.DateOnly}, {@link org.skyve.domain.types.DateTime},
 * {@link org.skyve.domain.types.Decimal2}, etc.). This package provides the
 * {@link jakarta.xml.bind.annotation.adapters.XmlAdapter} implementations that bridge
 * between the Skyve types and their XML/JAXB counterparts:
 * <ul>
 *   <li>{@code DateOnlyMapper}, {@code DateTimeMapper}, {@code TimeOnlyMapper},
 *       {@code TimestampMapper} — map Skyve temporal types to/from {@link java.util.Date}.
 *   <li>{@code Decimal2Mapper}, {@code Decimal5Mapper}, {@code Decimal10Mapper} — map
 *       Skyve {@code Decimal} types to/from {@link java.math.BigDecimal}.
 *   <li>{@code GeometryMapper} — maps JTS {@code Geometry} instances to/from their
 *       WKT string representation.
 *   <li>{@code OptimisticLockMapper} — maps {@link org.skyve.domain.types.OptimisticLock}
 *       to/from its string token.
 *   <li>{@link org.skyve.impl.domain.types.jaxb.CDATAAdapter} — wraps string values in
 *       CDATA sections to preserve formatting in XML output.
 * </ul>
 *
 * <p>These adapters are referenced via {@code @XmlJavaTypeAdapter} annotations on the
 * fields of generated domain classes and metadata model classes. They should not be
 * invoked directly.
 */
package org.skyve.impl.domain.types.jaxb;
