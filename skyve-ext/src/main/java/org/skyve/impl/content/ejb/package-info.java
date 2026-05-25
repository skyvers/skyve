/**
 * EJB-based remote content manager client and server stubs.
 *
 * <p>{@code AbstractEJBRemoteContentManagerClient} connects a Skyve instance to
 * a remote content manager exposed as an EJB. Callers subclass it and implement
 * {@code obtainServer()} to supply the remote bean reference.
 * {@code EJBRemoteContentManagerServer} / {@code AbstractEJBRemoteContentManagerServerBean}
 * provide the server-side counterpart.
 *
 * @see org.skyve.impl.content.rest
 */
package org.skyve.impl.content.ejb;
