/**
 * Defines the user, role, and permission model for Skyve's security layer.
 *
 * <p>The central type is {@link org.skyve.metadata.user.User}, which represents an
 * authenticated principal bound to a customer (tenant). The {@code User} provides all
 * runtime permission checks through its {@code can*} methods.
 *
 * <p>Access control is declared at the role level:
 * <ul>
 *   <li>{@link org.skyve.metadata.user.Role} groups document permissions within a module.</li>
 *   <li>{@link org.skyve.metadata.user.DocumentPermission} encodes the CRUD capabilities
 *       and data visibility scope as a single enum constant whose name is a five-character
 *       string (e.g. {@code CRUDG}, {@code _R__U}).</li>
 *   <li>{@link org.skyve.metadata.user.DocumentPermissionScope} defines the five visibility
 *       tiers: {@code none}, {@code user}, {@code dataGroup}, {@code customer},
 *       {@code global}.</li>
 * </ul>
 *
 * <p>{@link org.skyve.metadata.user.UserAccess} is an immutable value object used by the
 * router and servlet layer to describe what kind of resource access is being requested
 * (singular edit, list aggregate, report, content, etc.), allowing the security
 * infrastructure to perform a precise access check via
 * {@link org.skyve.metadata.user.User#canAccess(UserAccess, String)}.
 *
 * @see org.skyve.metadata.user.User
 * @see org.skyve.metadata.user.DocumentPermission
 * @see org.skyve.metadata.user.DocumentPermissionScope
 * @see org.skyve.metadata.user.UserAccess
 */
package org.skyve.metadata.user;
