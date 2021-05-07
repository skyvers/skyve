package org.skyve.impl.web.faces.beans;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;

import org.skyve.impl.util.UtilImpl;

@ManagedBean(name = "_skyveImage")
@RequestScoped
public class ImageUpload extends ContentUpload {
	private static final long serialVersionUID = 3614229213369205657L;

	public ImageUpload() {
    	super(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB);
    }
}
