package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <upload>} action button in a view.
 *
 * <p>An upload action invokes the server-side upload class identified by
 * {@link ClassAction#getClassName()} after the user selects a file, streaming
 * the upload to the handler for processing.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "upload")
public class UploadAction extends ClassAction {
	private static final long serialVersionUID = -116132937073829890L;

	/**
	 * Runtime action property key carrying the resolved upload capture affordance.
	 */
	public static final String CAPTURE_PROPERTY_NAME = "capture";

	private ContentCapture capture;

	/**
	 * Creates an upload action with the implicit upload action name.
	 */
	public UploadAction() {
		implicitName = ImplicitActionName.Upload;
	}

	/**
	 * Returns the explicit capture affordance configured for this upload action.
	 *
	 * @return the configured capture affordance, or {@code null} when metadata omitted
	 *         the attribute
	 * @see #getResolvedCapture()
	 */
	public @Nullable ContentCapture getCapture() {
		return capture;
	}

	/**
	 * Sets the explicit capture affordance for this upload action.
	 *
	 * @param capture the capture affordance, or {@code null} to use the default
	 *        {@link ContentCapture#none}
	 */
	@XmlAttribute(required = false)
	public void setCapture(@Nullable ContentCapture capture) {
		this.capture = capture;
	}

	/**
	 * Returns the effective capture affordance for this upload action.
	 *
	 * @return the configured capture affordance, or {@link ContentCapture#none} when
	 *         the XML attribute is absent
	 */
	public @Nonnull ContentCapture getResolvedCapture() {
		return (capture == null) ? ContentCapture.none : capture;
	}

	/**
	 * Converts this repository descriptor to runtime action metadata.
	 *
	 * <p>Side effects: stores the resolved capture affordance as an action property
	 * so renderer-only code can retain upload capture behavior without depending on
	 * repository JAXB classes.
	 *
	 * @return runtime action metadata; never {@code null}
	 */
	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.getProperties().put(CAPTURE_PROPERTY_NAME, getResolvedCapture().name());
		return result;
	}
}
