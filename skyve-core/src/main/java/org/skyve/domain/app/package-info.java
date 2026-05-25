/**
 * Root package for cross-cutting Skyve application domain types shared by all modules.
 *
 * <p>{@code AppConstants} defines framework-wide constants (module names, document
 * names, field name strings, and role names) used to reference the admin and other
 * built-in modules in a type-safe, refactoring-friendly way.
 *
 * <p>The {@code admin} sub-package contains interface types for the core admin
 * document domain objects (User, Contact, Tag, Communication, etc.) that downstream
 * modules can depend on without depending on the full admin module implementation.
 *
 * @see org.skyve.domain.app.admin
 */
package org.skyve.domain.app;
