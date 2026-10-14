package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <previousCompleteAccess>} access
 * constraint on a view; restricts the view to users with previous-complete
 * record access.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ViewUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "previousCompleteAccess")
public class ViewPreviousCompleteUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = 7559630279039840437L;

	private String binding;

	public @Nonnull String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(@Nonnull String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}
	
	@Override
	public void validate(@Nonnull String metaDataName, @Nonnull Module module) {
		if (binding == null) {
			throw new MetaDataException(metaDataName + " : [binding] is required for all previousComplete user accesses.");
		}
		// NB can't validate binding until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}

	@Override
	public @Nonnull UserAccess toUserAccess(@Nonnull String moduleName, @Nonnull String documentName) {
		return UserAccess.previousComplete(moduleName, documentName, binding);
	}
}
