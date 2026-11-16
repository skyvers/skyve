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
 * Converters Data Tables
 * <br/>
 * Non-persistent Kitchen Sink fixture that lists the converters fixtures in a PrimeFaces data table for each
			group of converters (each list grid's componentBuilderClass is faces), for filters typed as the cells show
			the values. The list views of the same queries are the Vue list grids (PrimeFaces) and SmartClient's.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ConvertersDataTables extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ConvertersDataTables";

	/** @hidden */
	public static final String notePropertyName = "note";

	/**
	 * Note
	 * <br/>
	 * Unused: a document needs an attribute.
	 **/
	private String note;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ConvertersDataTables.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ConvertersDataTables.DOCUMENT_NAME;
	}

	public static ConvertersDataTables newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Converters Data Tables", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #note} accessor.
	 * @return	The value.
	 **/
	public String getNote() {
		return note;
	}

	/**
	 * {@link #note} mutator.
	 * @param note	The new value.
	 **/
	@XmlElement
	public void setNote(String note) {
		preset(notePropertyName, note);
		this.note = note;
	}
}
