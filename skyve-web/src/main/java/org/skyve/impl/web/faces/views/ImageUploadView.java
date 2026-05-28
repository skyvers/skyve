package org.skyve.impl.web.faces.views;

import org.skyve.impl.util.UtilImpl;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;

/**
 * Models a view interaction and binds it to the active Skyve web context.
 */
@RequestScoped
@Named("_skyveImage")
public class ImageUploadView extends ContentUploadView {
	private static final long serialVersionUID = 3614229213369205657L;

	/**
	 * Creates the request-scoped image upload view with image-specific upload limits.
	 */
	public ImageUploadView() {
    	super(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB);
    }
}
