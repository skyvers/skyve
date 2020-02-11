package modules.test.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.domain.MappedBase;
import org.skyve.CORE;
import org.skyve.domain.PolymorphicPersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Mapped Extension
 * <br/>
 * Extension document.
 * 
 * @navhas n aggregatedCollection 0..n MappedExtensionJoinedStrategy
 * @navcomposed n composedCollection 0..n MappedExtensionJoinedStrategy
 * @navhas n aggregatedAssociation 0..1 MappedExtensionJoinedStrategy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@PolymorphicPersistentBean
public abstract class MappedExtensionJoinedStrategy extends MappedBase {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MappedExtensionJoinedStrategy";

	/** @hidden */
	public static final String aggregatedAssociationPropertyName = "aggregatedAssociation";
	/** @hidden */
	public static final String aggregatedCollectionPropertyName = "aggregatedCollection";
	/** @hidden */
	public static final String composedCollectionPropertyName = "composedCollection";
	/** @hidden */
	public static final String inverseAggregatedAssociationPropertyName = "inverseAggregatedAssociation";
	/** @hidden */
	public static final String derivedIntegerPropertyName = "derivedInteger";

	/**
	 * Aggregated Association
	 **/
	private MappedExtensionJoinedStrategyExtension aggregatedAssociation = null;
	/**
	 * Aggregated Collection
	 **/
	private List<MappedExtensionJoinedStrategyExtension> aggregatedCollection = new ChangeTrackingArrayList<>("aggregatedCollection", this);
	/**
	 * Composed Collection
	 **/
	private List<MappedExtensionJoinedStrategyExtension> composedCollection = new ChangeTrackingArrayList<>("composedCollection", this);
	/**
	 * Inverse
	 **/
	private List<MappedExtensionJoinedStrategyExtension> inverseAggregatedAssociation = new ArrayList<>();
	/**
	 * Derived Integer
	 **/
	private Integer derivedInteger;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedExtensionJoinedStrategy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedExtensionJoinedStrategy.DOCUMENT_NAME;
	}

	public static MappedExtensionJoinedStrategyExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{text}",
														this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MappedExtensionJoinedStrategy) && 
					this.getBizId().equals(((MappedExtensionJoinedStrategy) o).getBizId()));
	}

	/**
	 * {@link #aggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public MappedExtensionJoinedStrategyExtension getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * @param aggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setAggregatedAssociation(MappedExtensionJoinedStrategyExtension aggregatedAssociation) {
		preset(aggregatedAssociationPropertyName, aggregatedAssociation);
		this.aggregatedAssociation = aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategyExtension> getAggregatedCollection() {
		return aggregatedCollection;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionJoinedStrategyExtension getAggregatedCollectionElementById(String bizId) {
		return getElementById(aggregatedCollection, bizId);
	}

	/**
	 * {@link #aggregatedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setAggregatedCollectionElementById(String bizId, MappedExtensionJoinedStrategyExtension element) {
		setElementById(aggregatedCollection, element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param element	The element to add.
	 **/
	public void addAggregatedCollectionElement(MappedExtensionJoinedStrategyExtension element) {
		aggregatedCollection.add(element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addAggregatedCollectionElement(int index, MappedExtensionJoinedStrategyExtension element) {
		aggregatedCollection.add(index, element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeAggregatedCollectionElement(MappedExtensionJoinedStrategyExtension element) {
		return aggregatedCollection.remove(element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionJoinedStrategyExtension removeAggregatedCollectionElement(int index) {
		return aggregatedCollection.remove(index);
	}

	/**
	 * {@link #composedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategyExtension> getComposedCollection() {
		return composedCollection;
	}

	/**
	 * {@link #composedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionJoinedStrategyExtension getComposedCollectionElementById(String bizId) {
		return getElementById(composedCollection, bizId);
	}

	/**
	 * {@link #composedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setComposedCollectionElementById(String bizId, MappedExtensionJoinedStrategyExtension element) {
		setElementById(composedCollection, element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param element	The element to add.
	 **/
	public void addComposedCollectionElement(MappedExtensionJoinedStrategyExtension element) {
		composedCollection.add(element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addComposedCollectionElement(int index, MappedExtensionJoinedStrategyExtension element) {
		composedCollection.add(index, element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeComposedCollectionElement(MappedExtensionJoinedStrategyExtension element) {
		return composedCollection.remove(element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionJoinedStrategyExtension removeComposedCollectionElement(int index) {
		return composedCollection.remove(index);
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategyExtension> getInverseAggregatedAssociation() {
		return inverseAggregatedAssociation;
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionJoinedStrategyExtension getInverseAggregatedAssociationElementById(String bizId) {
		return getElementById(inverseAggregatedAssociation, bizId);
	}

	/**
	 * {@link #inverseAggregatedAssociation} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInverseAggregatedAssociationElementById(String bizId, MappedExtensionJoinedStrategyExtension element) {
		setElementById(inverseAggregatedAssociation, element);
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param element	The element to add.
	 **/
	public void addInverseAggregatedAssociationElement(MappedExtensionJoinedStrategyExtension element) {
		inverseAggregatedAssociation.add(element);
		element.setAggregatedAssociation((MappedExtensionJoinedStrategyExtension) this);
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInverseAggregatedAssociationElement(int index, MappedExtensionJoinedStrategyExtension element) {
		inverseAggregatedAssociation.add(index, element);
		element.setAggregatedAssociation((MappedExtensionJoinedStrategyExtension) this);
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInverseAggregatedAssociationElement(MappedExtensionJoinedStrategyExtension element) {
		boolean result = inverseAggregatedAssociation.remove(element);
		if (result) {
			element.setAggregatedAssociation(null);
		}
		return result;
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionJoinedStrategyExtension removeInverseAggregatedAssociationElement(int index) {
		MappedExtensionJoinedStrategyExtension result = inverseAggregatedAssociation.remove(index);
		result.setAggregatedAssociation(null);
		return result;
	}

	/**
	 * {@link #derivedInteger} accessor.
	 * @return	The value.
	 **/
	public Integer getDerivedInteger() {
		return derivedInteger;
	}

	/**
	 * {@link #derivedInteger} mutator.
	 * @param derivedInteger	The new value.
	 **/
	@XmlElement
	public void setDerivedInteger(Integer derivedInteger) {
		this.derivedInteger = derivedInteger;
	}
}
