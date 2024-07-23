package org.skyve.impl.web.faces.views;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;

/**
 * Backs the svgedit.xhtml page.
 */
@RequestScoped
@Named("_skyveSVG")
public class SVGEditView extends HarnessView {
	private static final long serialVersionUID = 8663003610034526320L;

	private String contentIdParameter;
	private String contentBindingParameter;
	private int widthParameter;
	private int heightParameter;
	
	public String getContentIdParameter() {
		return contentIdParameter;
	}

	public void setContentIdParameter(String contentIdParameter) {
		this.contentIdParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentIdParameter));
	}

	public String getContentBindingParameter() {
		return contentBindingParameter;
	}

	public void setContentBindingParameter(String contentBindingParameter) {
		this.contentBindingParameter = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(contentBindingParameter));
	}

	public int getWidthParameter() {
		return widthParameter;
	}

	public void setWidthParameter(int widthParameter) {
		this.widthParameter = widthParameter;
	}

	public int getHeightParameter() {
		return heightParameter;
	}

	public void setHeightParameter(int heightParameter) {
		this.heightParameter = heightParameter;
	}
	
	/**
	 * Provides the background raster image URL for the SVG-Editor.
	 * @return
	 */
	public String getBackgroundUrl() {
		StringBuilder result = new StringBuilder(64);
		result.append(getBaseHref()).append("content?_nm&_n=").append(contentIdParameter);
		result.append("&_doc=").append(getBizDocumentParameter());
		result.append("&_b=").append(BindUtil.unsanitiseBinding(contentBindingParameter));
		return result.toString();
	}
}
