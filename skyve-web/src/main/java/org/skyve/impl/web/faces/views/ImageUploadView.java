package org.skyve.impl.web.faces.views;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.annotation.ManagedProperty;
import jakarta.inject.Inject;
import jakarta.inject.Named;

/**
 * Models a view interaction and binds it to the active Skyve web context.
 */
@RequestScoped
@Named("_skyveImage")
public class ImageUploadView extends ContentUploadView {
	private static final long serialVersionUID = 3614229213369205657L;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.CAMERA_NAME + "}")
	@SuppressWarnings("java:S6813") // allow member injection
	private String camera;

	/**
	 * Creates the request-scoped image upload view with image-specific upload limits.
	 */
	public ImageUploadView() {
		super(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB);
	}

	@Override
	@PostConstruct
	public void postConstruct() {
		super.postConstruct();
		camera = String.valueOf(Boolean.parseBoolean(UtilImpl.processStringValue(camera)));
	}

	/**
	 * Returns whether the image upload view should prefer native camera capture.
	 */
	public boolean isCamera() {
		return Boolean.parseBoolean(camera);
	}
}
