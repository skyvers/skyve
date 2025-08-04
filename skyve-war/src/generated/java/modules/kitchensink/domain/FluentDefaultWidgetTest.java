package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * FluentDefaultWidget Test
 * <br/>
 * Non-persistent document used to test the functionality of the Default widget in the Fluent api
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class FluentDefaultWidgetTest extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "FluentDefaultWidgetTest";

	/** @hidden */
	public static final String generatedXmlPropertyName = "generatedXml";

	/** @hidden */
	public static final String skyveDocumentNamePropertyName = "skyveDocumentName";

	/** @hidden */
	public static final String previewDocumentPropertyName = "previewDocument";

	/**
	 * Generated xml
	 * <br/>
	 * Used to view the xml generated from the fluent document
	 **/
	private String generatedXml;

	/**
	 * Skyve document name
	 **/
	private String skyveDocumentName;

	/**
	 * Preview document
	 **/
	private Boolean previewDocument = Boolean.valueOf(false);

	@Override
	@XmlTransient
	public String getBizModule() {
		return FluentDefaultWidgetTest.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return FluentDefaultWidgetTest.DOCUMENT_NAME;
	}

	public static FluentDefaultWidgetTest newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("FluentDefaultWidgetTest", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #generatedXml} accessor.
	 * @return	The value.
	 **/
	public String getGeneratedXml() {
		return generatedXml;
	}

	/**
	 * {@link #generatedXml} mutator.
	 * @param generatedXml	The new value.
	 **/
	@XmlElement
	public void setGeneratedXml(String generatedXml) {
		preset(generatedXmlPropertyName, generatedXml);
		this.generatedXml = generatedXml;
	}

	/**
	 * {@link #skyveDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getSkyveDocumentName() {
		return skyveDocumentName;
	}

	/**
	 * {@link #skyveDocumentName} mutator.
	 * @param skyveDocumentName	The new value.
	 **/
	@XmlElement
	public void setSkyveDocumentName(String skyveDocumentName) {
		preset(skyveDocumentNamePropertyName, skyveDocumentName);
		this.skyveDocumentName = skyveDocumentName;
	}

	/**
	 * {@link #previewDocument} accessor.
	 * @return	The value.
	 **/
	public Boolean getPreviewDocument() {
		return previewDocument;
	}

	/**
	 * {@link #previewDocument} mutator.
	 * @param previewDocument	The new value.
	 **/
	@XmlElement
	public void setPreviewDocument(Boolean previewDocument) {
		preset(previewDocumentPropertyName, previewDocument);
		this.previewDocument = previewDocument;
	}

	/**
	 * showPreviewDocument
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowPreviewDocument() {
		return (Boolean.TRUE.equals(this.getPreviewDocument()));
	}

	/**
	 * {@link #isShowPreviewDocument} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowPreviewDocument() {
		return (! isShowPreviewDocument());
	}
}
