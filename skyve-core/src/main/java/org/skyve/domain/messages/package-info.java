/**
 * Exception hierarchy and message types used throughout the Skyve framework.
 *
 * <h2>Exception hierarchy</h2>
 * <pre>
 * RuntimeException
 *  └─ {@link org.skyve.domain.messages.SkyveException}  (i18n-aware base)
 *      └─ {@link org.skyve.domain.messages.DomainException}  (general framework exception)
 *          ├─ {@link org.skyve.domain.messages.ValidationException}
 *          ├─ {@link org.skyve.domain.messages.ConversionException}
 *          ├─ {@link org.skyve.domain.messages.OptimisticLockException}
 *          ├─ {@link org.skyve.domain.messages.ConversationEndedException}
 *          ├─ {@link org.skyve.domain.messages.SessionEndedException}
 *          ├─ {@link org.skyve.domain.messages.ManyResultsException}
 *          ├─ {@link org.skyve.domain.messages.NoResultsException}
 *          ├─ {@link org.skyve.domain.messages.ReferentialConstraintViolationException}
 *          ├─ {@link org.skyve.domain.messages.UniqueConstraintViolationException}
 *          └─ {@link org.skyve.domain.messages.TimeoutException}
 *      └─ {@link org.skyve.domain.messages.UploadException}  (warnings + errors)
 * </pre>
 *
 * <h2>I18N support</h2>
 * <p>All {@link org.skyve.domain.messages.SkyveException} constructors pass message strings
 * through {@link org.skyve.util.Util#nullSafeI18n} before forwarding to
 * {@link RuntimeException}. This means message strings may be either literal text or
 * i18n resource keys; the framework resolves them against the current user's locale
 * at the time of construction.
 *
 * <h2>Multi-message exceptions</h2>
 * <p>Exceptions that implement {@link org.skyve.domain.messages.MessageException} carry a
 * list of {@link org.skyve.domain.messages.Message} objects. Each message has user-readable
 * text and zero or more bean-binding paths that the UI uses to highlight specific form
 * fields. The framework accumulates all messages before displaying them, so users see all
 * errors at once rather than one at a time.
 *
 * <h2>Key types for application code</h2>
 * <ul>
 *   <li>{@link org.skyve.domain.messages.ValidationException} — thrown from
 *       {@link org.skyve.metadata.model.document.Bizlet#validate} to report one or more
 *       field-level or cross-field constraint violations.
 *   <li>{@link org.skyve.domain.messages.Message} — the unit of information passed to the
 *       UI; carries text and optional field bindings for inline error highlighting.
 *   <li>{@link org.skyve.domain.messages.DomainException} — thrown for general
 *       unrecoverable domain errors; also serves as the base class for all specialised
 *       exceptions.
 * </ul>
 */
package org.skyve.domain.messages;
