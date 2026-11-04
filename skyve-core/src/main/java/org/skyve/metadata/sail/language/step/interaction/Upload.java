package org.skyve.metadata.sail.language.step.interaction;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Uploads a classpath file, either into the content widget bound to {@code binding}
 * or through the upload action named {@code action}.
 *
 * <ul>
 *   <li>{@code <upload binding="content" file="sail/files/logo.png"/>} supplies the file to
 *       the {@code content} widget of the current edit view.</li>
 *   <li>{@code <upload action="ImportAction" file="sail/files/data.csv"/>} presses the upload
 *       action's button, supplies the file to the upload dialog and submits it.</li>
 * </ul>
 *
 * <p>Exactly one of {@code binding} and {@code action} is required; the XSD cannot express
 * this, so {@link #execute(Executor)} checks it. {@code file} is a classpath resource path
 * resolved by the executor (in the test JVM), not a path on the server.
 *
 * @see DataEnter
 * @see org.skyve.metadata.sail.execution.Executor#executeUpload
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Upload implements Step {

	private String binding;
	private String actionName;
	private String file;

	/**
	 * Returns the binding of the content widget that receives the file.
	 * @return the binding, or {@code null} when the file is uploaded through {@link #getActionName()}
	 */
	public @Nullable String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the content widget that receives the file.
	 * @param binding the binding, or {@code null} to upload through an action
	 */
	@XmlAttribute(name = "binding")
	public void setBinding(@Nullable String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the name of the upload action that receives the file.
	 * @return the action name, or {@code null} when the file is uploaded into {@link #getBinding()}
	 */
	public @Nullable String getActionName() {
		return actionName;
	}

	/**
	 * Sets the name of the upload action that receives the file.
	 * @param actionName the action name, or {@code null} to upload into a content widget
	 */
	@XmlAttribute(name = "action")
	public void setActionName(@Nullable String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}

	/**
	 * Returns the classpath resource path of the file to upload.
	 * @return the resource path, for example {@code sail/files/logo.png}
	 */
	public @Nonnull String getFile() {
		return file;
	}

	/**
	 * Sets the classpath resource path of the file to upload.
	 * @param file the resource path, for example {@code sail/files/logo.png}
	 */
	@XmlAttribute(name = "file", required = true)
	public void setFile(@Nonnull String file) {
		this.file = UtilImpl.processStringValue(file);
	}

	/**
	 * Checks that exactly one target is set, then dispatches to {@link Executor#executeUpload(Upload)}.
	 * @param executor the executor
	 * @throws MetaDataException if neither or both of {@code binding} and {@code action} are set
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if ((binding == null) == (actionName == null)) {
			throw new MetaDataException("<Upload /> requires exactly one of [binding] or [action]");
		}
		executor.executeUpload(this);
	}

	/**
	 * Returns the binding or the action name, whichever is set.
	 * @param context the context
	 * @return the binding or action name
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return (binding != null) ? binding : actionName;
	}
}
