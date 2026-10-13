package org.skyve.impl.metadata.model.document;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Association;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "association")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "association", 
			propOrder = {"requiredBool", "requiredMessage", "type", "embeddedColumnsPrefix", "databaseIndex"})
/**
 * JAXB-annotated implementation of {@link Association}, representing a many-to-one
 * or aggregation reference from one document to another.
 *
 * <p>Carries the {@link AssociationType} (aggregation or composition) and
 * the query name to use when presenting pick lists for this association.
 * An association attribute is stored as a foreign key column in the owning
 * document's table.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ReferenceImpl
 * @see Association
 */
public class AssociationImpl extends ReferenceImpl implements Association {
	private static final long serialVersionUID = -2839713495173145591L;

	private boolean required;
	private String requiredMessage;
	private AssociationType type;
	private String embeddedColumnsPrefix;
	private Boolean databaseIndex;

	public AssociationImpl() {
		setAttributeType(AttributeType.association);
	}

	@Override
	public @Nullable AssociationType getType() {
		return type;
	}

	@XmlAttribute(required = true)
	public void setType(@Nullable AssociationType type) {
		this.type = type;
	}
	
	@Override
	public boolean isRequired() {
		return required;
	}
	
	public @Nonnull Boolean getRequiredBool() {
		return Boolean.valueOf(required);
	}

	@XmlTransient
	public void setRequired(boolean required) {
		this.required = required;
	}
	
	@XmlAttribute(name = "required", required = false)
	public void setRequiredBool(@Nonnull Boolean required) {
		this.required = required.booleanValue();
	}
	
	@Override
	public @Nullable String getRequiredMessage() {
		return requiredMessage;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setRequiredMessage(@Nullable String requiredMessage) {
		this.requiredMessage = UtilImpl.processStringValue(requiredMessage);
	}

	@Override
	public @Nullable String getEmbeddedColumnsPrefix() {
		return embeddedColumnsPrefix;
	}

	@XmlAttribute(required = false)
	public void setEmbeddedColumnsPrefix(@Nullable String embeddedColumnsPrefix) {
		this.embeddedColumnsPrefix = embeddedColumnsPrefix;
	}
	
	@Override
	public @Nullable Boolean getDatabaseIndex() {
		return databaseIndex;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setDatabaseIndex(@Nullable Boolean databaseIndex) {
		this.databaseIndex = databaseIndex;
	}
}
