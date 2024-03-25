package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * All Dynamic Attributes Persistent
 * <br/>
 * All dynamic attributes in a persistent document.
 * 
 * @navcomposed n composedCollection 0..n AllDynamicAttributesPersistent
 * @navhas n dynamicAggregatedCollection 0..n AllAttributesDynamicPersistent
 * @navcomposed n dynamicEmbeddedAssociation 0..1 AllAttributesDynamicEmbedded
 * @navcomposed n composedAssociation 0..1 AllDynamicAttributesPersistent
 * @navhas n aggregatedAssociation 0..1 AllDynamicAttributesPersistent
 * @navhas n dynamicAggregatedAssociation 0..1 AllAttributesDynamicPersistent
 * @navcomposed n dynamicComposedCollection 0..n AllAttributesDynamicPersistent
 * @navcomposed n dynamicComposedAssociation 0..1 AllAttributesDynamicPersistent
 * @navcomposed 1 dynamicChildCollection 0..n AllDynamicAttributesPersistentDynamicChild
 * @navcomposed n embeddedAssociation 0..1 AllAttributesEmbedded
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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
	public static final String composedCollectionPropertyName = "composedCollection";

	/** @hidden */
	public static final String dynamicAggregatedCollectionPropertyName = "dynamicAggregatedCollection";

	/** @hidden */
	public static final String dynamicComposedCollectionPropertyName = "dynamicComposedCollection";

	/** @hidden */
	public static final String dynamicChildCollectionPropertyName = "dynamicChildCollection";

	/** @hidden */
	public static final String booleanFlagPropertyName = "booleanFlag";

	/** @hidden */
	public static final String colourPropertyName = "colour";

	/** @hidden */
	public static final String datePropertyName = "date";

	/** @hidden */
	public static final String dateTimePropertyName = "dateTime";

	/** @hidden */
	public static final String decimal10PropertyName = "decimal10";

	/** @hidden */
	public static final String decimal2PropertyName = "decimal2";

	/** @hidden */
	public static final String decimal5PropertyName = "decimal5";

	/** @hidden */
	public static final String enum3PropertyName = "enum3";

	/** @hidden */
	public static final String geometryPropertyName = "geometry";

	/** @hidden */
	public static final String idPropertyName = "id";

	/** @hidden */
	public static final String normalIntegerPropertyName = "normalInteger";

	/** @hidden */
	public static final String inverseAggregatedAssociationPropertyName = "inverseAggregatedAssociation";

	/** @hidden */
	public static final String dynamicInverseAggregatedAssociationPropertyName = "dynamicInverseAggregatedAssociation";

	/** @hidden */
	public static final String longIntegerPropertyName = "longInteger";

	/** @hidden */
	public static final String markupPropertyName = "markup";

	/** @hidden */
	public static final String memoPropertyName = "memo";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String timePropertyName = "time";

	/** @hidden */
	public static final String timestampPropertyName = "timestamp";

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
	 * Composed Collection
	 **/
	private List<AllDynamicAttributesPersistent> composedCollection = new ChangeTrackingArrayList<>("composedCollection", this);

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
	 * {@link #composedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllDynamicAttributesPersistent> getComposedCollection() {
		return composedCollection;
	}

	/**
	 * {@link #composedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllDynamicAttributesPersistent getComposedCollectionElementById(String bizId) {
		return getElementById(composedCollection, bizId);
	}

	/**
	 * {@link #composedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setComposedCollectionElementById(String bizId, AllDynamicAttributesPersistent element) {
		setElementById(composedCollection, element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addComposedCollectionElement(AllDynamicAttributesPersistent element) {
		return composedCollection.add(element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addComposedCollectionElement(int index, AllDynamicAttributesPersistent element) {
		composedCollection.add(index, element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeComposedCollectionElement(AllDynamicAttributesPersistent element) {
		return composedCollection.remove(element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllDynamicAttributesPersistent removeComposedCollectionElement(int index) {
		return composedCollection.remove(index);
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
