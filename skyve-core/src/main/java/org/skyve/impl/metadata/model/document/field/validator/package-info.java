/**
 * Internal validation constraints that can be attached to scalar field types.
 *
 * <p>Each {@code Validator} subclass in this package enforces a domain-level constraint
 * on a specific attribute type:
 * <ul>
 *   <li>{@code FieldValidator} — base class; accumulates
 *       {@link org.skyve.domain.messages.Message} objects into a
 *       {@link org.skyve.domain.messages.ValidationException} rather than throwing
 *       immediately.
 *   <li>{@code TextValidator} — validates {@code String} values against an optional
 *       regex pattern and length constraints.
 *   <li>{@code DateValidator} — validates {@link org.skyve.domain.types.DateOnly} or
 *       related date values against a min/max range.
 *   <li>{@code DecimalValidator} — validates {@link org.skyve.domain.types.Decimal2} /
 *       {@code Decimal5} / {@code Decimal10} values.
 *   <li>{@code IntegerValidator} / {@code LongValidator} — validate integer and long
 *       integer values.
 *   <li>{@code RangeValidator} — generic ordered-type range check shared by numeric and
 *       date validators.
 * </ul>
 *
 * <p>Validators are associated with {@link org.skyve.impl.metadata.model.document.field.ConstrainableField}
 * instances and invoked by the framework binding pipeline during value submission.
 *
 * @see org.skyve.domain.types.converters.Validator
 */
package org.skyve.impl.metadata.model.document.field.validator;
