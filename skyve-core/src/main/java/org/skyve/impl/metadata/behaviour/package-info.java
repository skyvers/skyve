/**
 * Implementation of server-side action behaviour: scripted method invocation and
 * conditional logic for Skyve metadata-driven actions.
 *
 * <p>This package provides the execution model for behaviour defined in
 * {@code Bizlet} extensions and server-side action metadata. Key types:
 * <ul>
 *   <li>{@code ServerSideMetaDataAction} — base for action classes whose logic is
 *       expressed in metadata (invoke, set, if) rather than handwritten Java.
 *   <li>{@code InvokeStatement} / {@code InvokeStaticStatement} — models a method
 *       invocation step in an action script, resolving the target object or class and
 *       its arguments at runtime.
 *   <li>{@code SetStatement} — models a binding-assignment step that sets a bean
 *       attribute to a computed value.
 *   <li>{@code IfStatement} — models a conditional branch in an action script.
 *   <li>{@code MethodArgument} — value holder for a single argument passed to an
 *       {@code InvokeStatement}.
 * </ul>
 *
 * @see org.skyve.impl.metadata.repository.behaviour
 */
package org.skyve.impl.metadata.behaviour;
