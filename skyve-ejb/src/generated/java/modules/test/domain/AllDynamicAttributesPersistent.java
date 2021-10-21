package modules.test.domain;

import java.util.ArrayList;
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
 * All Dynamic Attributes Persistent
 * <br/>
 * All dynamic attributes in a persistent document.
 * 
 * @navhas n aggregatedCollection 0..n AllAttributesPersistent
 * @navcomposed n dynamicEmbeddedAssociation 0..1 AllAttributesDynamicEmbedded
 * @navcomposed n composedAssociation 0..1 AllDynamicAttributesPersistent
 * @navhas n aggregatedAssociation 0..1 AllDynamicAttributesPersistent
 * @navhas n dynamicAggregatedAssociation 0..1 AllAttributesDynamicPersistent
 * @navcomposed n dynamicComposedAssociation 0..1 AllAttributesDynamicPersistent
 * @navcomposed n embeddedAssociation 0..1 AllAttributesEmbedded
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class AllDynamicAttributesPersistent extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "AllDynamicAttributesPersistent";

	/** @hidden */
	public static final String aggregatedAssociationPropertyName = "aggregatedAssociation";

	/** @hidden */
	public static final String composedAssociationPropertyName = "composedAssociation";

	/** @hidden */
	public static final String embeddedAssociationPropertyName = "embeddedAssociation";

	/** @hidden */
	public static final String dynamicAggregatedAssociationPropertyName = "dynamicAggregatedAssociation";

	/** @hidden */
	public static final String dynamicComposedAssociationPropertyName = "dynamicComposedAssociation";

	/** @hidden */
	public static final String dynamicEmbeddedAssociationPropertyName = "dynamicEmbeddedAssociation";

	/** @hidden */
	public static final String aggregatedCollectionPropertyName = "aggregatedCollection";

	/** @hidden */
	public static final String inverseAggregatedAssociationPropertyName = "inverseAggregatedAssociation";

	/** @hidden */
	public static final String dynamicInverseAggregatedAssociationPropertyName = "dynamicInverseAggregatedAssociation";

	/**
	 * Aggregated Association
	 **/
	private AllDynamicAttributesPersistent aggregatedAssociation = null;

	/**
	 * Composed Association
	 **/
	private AllDynamicAttributesPersistent composedAssociation = null;

	/**
	 * Embedded Association
	 **/
	private AllAttributesEmbedded embeddedAssociation = null;

	/**
	 * Aggregated Collection
	 **/
	private List<AllAttributesPersistent> aggregatedCollection = new ChangeTrackingArrayList<>("aggregatedCollection", this);

	/**
	 * Inverse
	 **/
	private List<AllDynamicAttributesPersistent> inverseAggregatedAssociation = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return AllDynamicAttributesPersistent.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return AllDynamicAttributesPersistent.DOCUMENT_NAME;
	}

	public static AllDynamicAttributesPersistent newInstance() {
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
		return ((o instanceof AllDynamicAttributesPersistent) && 
					this.getBizId().equals(((AllDynamicAttributesPersistent) o).getBizId()));
	}

	/**
	 * {@link #aggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllDynamicAttributesPersistent getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * @param aggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setAggregatedAssociation(AllDynamicAttributesPersistent aggregatedAssociation) {
		if (this.aggregatedAssociation != aggregatedAssociation) {
			preset(aggregatedAssociationPropertyName, aggregatedAssociation);
			AllDynamicAttributesPersistent oldAggregatedAssociation = this.aggregatedAssociation;
			this.aggregatedAssociation = aggregatedAssociation;
			if ((aggregatedAssociation != null) && (aggregatedAssociation.getInverseAggregatedAssociationElementById(getBizId()) == null)) {
				aggregatedAssociation.getInverseAggregatedAssociation().add(this);
			}
			if (oldAggregatedAssociation != null) {
				oldAggregatedAssociation.getInverseAggregatedAssociation().remove(this);
			}
		}
	}

	public void nullAggregatedAssociation() {
		this.aggregatedAssociation = null;
	}

	/**
	 * {@link #composedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllDynamicAttributesPersistent getComposedAssociation() {
		return composedAssociation;
	}

	/**
	 * {@link #composedAssociation} mutator.
	 * @param composedAssociation	The new value.
	 **/
	@XmlElement
	public void setComposedAssociation(AllDynamicAttributesPersistent composedAssociation) {
		if (this.composedAssociation != composedAssociation) {
			preset(composedAssociationPropertyName, composedAssociation);
			this.composedAssociation = composedAssociation;
		}
	}

	/**
	 * {@link #embeddedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesEmbedded getEmbeddedAssociation() {
		return embeddedAssociation;
	}

	/**
	 * {@link #embeddedAssociation} mutator.
	 * @param embeddedAssociation	The new value.
	 **/
	@XmlElement
	public void setEmbeddedAssociation(AllAttributesEmbedded embeddedAssociation) {
		if (this.embeddedAssociation != embeddedAssociation) {
			preset(embeddedAssociationPropertyName, embeddedAssociation);
			this.embeddedAssociation = embeddedAssociation;
		}
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllAttributesPersistent> getAggregatedCollection() {
		return aggregatedCollection;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllAttributesPersistent getAggregatedCollectionElementById(String bizId) {
		return getElementById(aggregatedCollection, bizId);
	}

	/**
	 * {@link #aggregatedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setAggregatedCollectionElementById(String bizId, AllAttributesPersistent element) {
		setElementById(aggregatedCollection, element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addAggregatedCollectionElement(AllAttributesPersistent element) {
		return aggregatedCollection.add(element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addAggregatedCollectionElement(int index, AllAttributesPersistent element) {
		aggregatedCollection.add(index, element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeAggregatedCollectionElement(AllAttributesPersistent element) {
		return aggregatedCollection.remove(element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllAttributesPersistent removeAggregatedCollectionElement(int index) {
		return aggregatedCollection.remove(index);
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllDynamicAttributesPersistent> getInverseAggregatedAssociation() {
		return inverseAggregatedAssociation;
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllDynamicAttributesPersistent getInverseAggregatedAssociationElementById(String bizId) {
		return getElementById(inverseAggregatedAssociation, bizId);
	}

	/**
	 * {@link #inverseAggregatedAssociation} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInverseAggregatedAssociationElementById(String bizId, AllDynamicAttributesPersistent element) {
		setElementById(inverseAggregatedAssociation, element);
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param element	The element to add.
	 **/
	public boolean addInverseAggregatedAssociationElement(AllDynamicAttributesPersistent element) {
		boolean result = false;
		if (getElementById(inverseAggregatedAssociation, element.getBizId()) == null) {
			result = inverseAggregatedAssociation.add(element);
		}
		element.setAggregatedAssociation(this);
		return result;
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInverseAggregatedAssociationElement(int index, AllDynamicAttributesPersistent element) {
		inverseAggregatedAssociation.add(index, element);
		element.setAggregatedAssociation(this);
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInverseAggregatedAssociationElement(AllDynamicAttributesPersistent element) {
		boolean result = inverseAggregatedAssociation.remove(element);
		if (result) {
			element.nullAggregatedAssociation();
		}
		return result;
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllDynamicAttributesPersistent removeInverseAggregatedAssociationElement(int index) {
		AllDynamicAttributesPersistent result = inverseAggregatedAssociation.remove(index);
		result.nullAggregatedAssociation();
		return result;
	}
}
