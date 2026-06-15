package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for query reference/import descriptors in a module.
 *
 * <p>Identifies a query defined in another module that this module imports
 * for reuse.  Concrete subclasses ({@link MetaDataQueryReferenceMetaData},
 * {@link SQLReferenceMetaData}, {@link BizQLReferenceMetaData}) specify the
 * query type.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see MetaDataQueryReferenceMetaData
 * @see SQLReferenceMetaData
 * @see BizQLReferenceMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"moduleRef", "ref"})
public abstract class QueryReferenceMetaData extends QueryMetaData {
	private static final long serialVersionUID = -2805157680604867709L;

	private String moduleRef;
	private String ref;
	
	public String getRef() {
		return ref;
	}

	@XmlAttribute(required = true)
	public void setRef(String ref) {
		this.ref = UtilImpl.processStringValue(ref);
	}

	public String getModuleRef() {
		return moduleRef;
	}

	@XmlAttribute(required = true)
	public void setModuleRef(String moduleRef) {
		this.moduleRef = UtilImpl.processStringValue(moduleRef);
	}
}
