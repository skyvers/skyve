/**
 * Internal implementations of every Skyve scalar field type, plus the field
 * base-class hierarchy.
 *
 * <p>Each class in this package corresponds to one
 * {@link org.skyve.metadata.model.Attribute.AttributeType} value. The inheritance
 * hierarchy is:
 * <pre>
 * AbstractAttribute
 *   └─ Field
 *        ├─ ConstrainableField          (adds min/max/required)
 *        │    ├─ ConvertibleField       (adds Converter)
 *        │    │    ├─ Date, DateTime, Time, Timestamp
 *        │    │    ├─ Decimal2, Decimal5, Decimal10
 *        │    │    └─ Integer, LongInteger
 *        │    ├─ LengthField            (adds length)
 *        │    │    ├─ Text
 *        │    │    ├─ Markup, Memo
 *        │    │    └─ Enumeration
 *        │    ├─ Boolean
 *        │    ├─ Colour
 *        │    ├─ Content, Image
 *        │    ├─ Geometry
 *        │    └─ Id
 *        └─ TextFormat                  (shared format/regex helper)
 * </pre>
 *
 * <p>These classes are JAXB-annotated and populated during repository loading from
 * document XML files. They are internal; callers access field metadata through
 * {@link org.skyve.metadata.model.Attribute} and
 * {@link org.skyve.metadata.model.document.Document#getAttributes()}.
 *
 * @see org.skyve.metadata.model.Attribute
 * @see org.skyve.metadata.model.document.Document
 */
package org.skyve.impl.metadata.model.document.field;
