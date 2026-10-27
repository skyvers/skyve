/**
 * Server-side dialogs: questions that server code asks the user during a request.
 *
 * <p>{@link org.skyve.dialog.Dialogs} is the service facade, obtained through
 * {@link org.skyve.EXT#getDialogs()}. Server code builds a {@link org.skyve.dialog.Question}
 * and asks it. When the request carries no answer, {@link org.skyve.dialog.QuestionException}
 * is thrown, the renderer rolls the request back and shows the question, and the client sends
 * the same request again with the answers given so far.
 *
 * @see org.skyve.dialog.Dialogs
 * @see org.skyve.dialog.Question
 * @see org.skyve.dialog.QuestionException
 * @since 10.0
 */
package org.skyve.dialog;
