package modules.test.domain;

import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * Hierarchical
 * <br/>
 * Hierarchical document.
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class Hierarchical extends AbstractPersistentBean implements HierarchicalBean<Hierarchical> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Hierarchical";

	/** @hidden */
	public static final String textPropertyName = "text";

	/**
	 * Text
	 **/
	private String text;

	private String bizParentId;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Hierarchical.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Hierarchical.DOCUMENT_NAME;
	}

	public static Hierarchical newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Hierarchical) && 
					this.getBizId().equals(((Hierarchical) o).getBizId()));
	}

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	@Override
	public String getBizParentId() {
		return bizParentId;
	}

	@Override
	@XmlElement
	public void setBizParentId(String bizParentId) {
		preset(HierarchicalBean.PARENT_ID, bizParentId);
		this.bizParentId = bizParentId;
	}

	@Override
	public Hierarchical getParent() {
		Hierarchical result = null;

		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(Hierarchical.MODULE_NAME, Hierarchical.DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}

		return result;
	}

	@Override
	@XmlTransient
	public List<Hierarchical> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Hierarchical.MODULE_NAME, Hierarchical.DOCUMENT_NAME);
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
