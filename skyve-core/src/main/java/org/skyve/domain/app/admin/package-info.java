/**
 * Interface contracts for the Skyve admin module's core domain objects.
 *
 * <p>This package contains one interface per admin document, providing a
 * dependency-inversion point so that framework code and downstream modules can
 * refer to admin document types (User, Contact, Communication, Tag, etc.) without
 * importing the concrete generated domain classes from {@code skyve-war}.
 *
 * <p>Each interface exposes the document's business getters and setters. The concrete
 * generated classes in {@code skyve-war/src/generated/java} implement these interfaces.
 *
 * @see org.skyve.domain.app
 */
package org.skyve.domain.app.admin;
