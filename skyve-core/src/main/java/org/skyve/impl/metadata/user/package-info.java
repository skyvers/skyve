/**
 * Internal implementations of the {@link org.skyve.metadata.user} user and security
 * contracts.
 *
 * <p>This package contains the runtime user model and permission evaluation:
 * <ul>
 *   <li>{@code UserImpl} — implements {@link org.skyve.metadata.user.User}; holds the
 *       authenticated user's identity, roles, data-group membership, and resolved
 *       privilege set. Constructed by the persistence layer on login.
 *   <li>{@code SuperUser} — a special {@code UserImpl} subclass with all privileges
 *       granted; used for framework background jobs and system operations that run
 *       outside a real user session.
 *   <li>{@code ClientUserData} — a serialisable snapshot of the user's identity data
 *       transferred to client-side code.
 *   <li>{@code RoleImpl} — runtime implementation of
 *       {@link org.skyve.metadata.user.Role}; carries the role's document and action
 *       privilege set.
 *   <li>{@code DocumentPrivilege} / {@code ActionPrivilege} / {@code Privilege} —
 *       value types representing the granular permissions granted by a role.
 *   <li>{@code AccessProcessor} — evaluates whether a user has a specific permission
 *       for a document or action, applying scope (global, customer, datagroup, user)
 *       and CRUD flags.
 * </ul>
 *
 * @see org.skyve.metadata.user.User
 * @see org.skyve.metadata.user.Role
 */
package org.skyve.impl.metadata.user;
