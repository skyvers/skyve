package modules.admin.domain;

import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Dynamic Entity
 * 
 * @navcomposed 1 relations 0..n DynamicRelation
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DynamicEntity extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DynamicEntity";

	/** @hidden */
	public static final String jsonPropertyName = "json";

	/** @hidden */
	public static final String relationsPropertyName = "relations";

	/**
	 * Name
	 **/
	private String json;

	/**
	 * Relations
	 **/
	private List<DynamicRelation> relations = new ChangeTrackingArrayList<>("relations", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return DynamicEntity.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DynamicEntity.DOCUMENT_NAME;
	}

	public static DynamicEntity newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{bizModule}.{bizDocument}#{bizId}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DynamicEntity) && 
					this.getBizId().equals(((DynamicEntity) o).getBizId()));
	}

	/**
	 * {@link #json} accessor.
	 * @return	The value.
	 **/
	public String getJson() {
		return json;
	}

	/**
	 * {@link #json} mutator.
	 * @param json	The new value.
	 **/
	@XmlElement
	public void setJson(String json) {
		preset(jsonPropertyName, json);
		this.json = json;
	}

	/**
	 * {@link #relations} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DynamicRelation> getRelations() {
		return relations;
	}

	/**
	 * {@link #relations} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DynamicRelation getRelationsElementById(String bizId) {
		return getElementById(relations, bizId);
	}

	/**
	 * {@link #relations} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRelationsElementById(String bizId, DynamicRelation element) {
		setElementById(relations, element);
	}

	/**
	 * {@link #relations} add.
	 * @param element	The element to add.
	 **/
	public boolean addRelationsElement(DynamicRelation element) {
		boolean result = relations.add(element);
		element.setParent(this);
		return result;
	}

	/**
	 * {@link #relations} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRelationsElement(int index, DynamicRelation element) {
		relations.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #relations} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRelationsElement(DynamicRelation element) {
		boolean result = relations.remove(element);
		element.setParent(null);
		return result;
	}

	/**
	 * {@link #relations} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public DynamicRelation removeRelationsElement(int index) {
		DynamicRelation result = relations.remove(index);
		result.setParent(null);
		return result;
	}
}
