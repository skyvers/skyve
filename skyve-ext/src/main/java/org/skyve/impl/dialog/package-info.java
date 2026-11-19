/**
 * Provides the request-based implementation of server-side dialogs and its singleton holder.
 *
 * <p>{@link org.skyve.impl.dialog.DefaultDialogs} reads answers from, and caches them on, the HTTP
 * request bound to the current thread. {@link org.skyve.impl.dialog.DialogsStaticSingleton} holds the
 * implementation that {@link org.skyve.EXT#getDialogs()} returns. {@link org.skyve.impl.dialog.DialogsProtocol} holds
 * what the renderers, their clients and any implementation share: marking a request as able to ask, the answers
 * parameter and the JSON a question is sent as.
 *
 * @since 10.0
 */
package org.skyve.impl.dialog;
