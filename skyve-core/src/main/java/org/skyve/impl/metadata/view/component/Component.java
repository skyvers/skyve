package org.skyve.impl.metadata.view.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.View;
import org.skyve.util.Binder.TargetMetaData;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"moduleName", 
							"documentName", 
							"name",
							"widgetId",
							"names",
							"properties"})
public class Component extends AbstractBound implements NamedMetaData, DecoratedMetaData, Invisible {
	private static final long serialVersionUID = 7882200042806155928L;

	private String moduleName;
	private String documentName;
	private String name;
	private String widgetId;

	private String invisibleConditionName;
	
	private List<MetaData> contained = null;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@XmlElementWrapper(name = "names", namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlElement(name = "name", namespace = XMLMetaData.VIEW_NAMESPACE)
	private List<ComponentNameMap> names = new ArrayList<>();

	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = false)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document", required = false)
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute(name = "name")
	public void setName(String name) {
		this.name = name;
	}

	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(name = "widgetId")
	public void setWidgetId(String widgetId) {
		this.widgetId = widgetId;
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible")
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "visible")
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	public List<ComponentNameMap> getNames() {
		return names;
	}

	public List<MetaData> getContained() {
		return contained;
	}

	public void setContained(String uxui, CustomerImpl customer, ModuleImpl owningModule, DocumentImpl owningDocument, String viewName) {
		String binding = getBinding();
		ModuleImpl m = null;
		DocumentImpl d = null;
		if (binding != null) {
			try {
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, owningModule, owningDocument, binding);
				Association a = (Association) target.getAttribute();
				d = (DocumentImpl) owningModule.getDocument(customer, a.getDocumentName());
				m = (ModuleImpl) customer.getModule(d.getOwningModuleName());
			}
			catch (Exception e) {
				throw new MetaDataException("Component" + ((name != null) ? " named " + name : "") +
												" with binding " + binding + " in view " + viewName + " for document " +
												owningModule.getName() + '.' + owningDocument.getName() +
												" for uxui " + uxui + " has an invalid binding.",
												e);
			}
		}
		else {
			if (moduleName != null) {
				m = (ModuleImpl) customer.getModule(moduleName);
			}
			else {
				m = owningModule;
			}
			if (documentName != null) {
				d = (DocumentImpl) m.getDocument(customer, documentName);
			}
			else {
				d = owningDocument;
			}
		}
		View originalView = (name == null) ? 
								d.getView(uxui, customer, viewName) : 
								d.getView(uxui, customer, name);
		if (originalView == null) {
			throw new MetaDataException("Component named " + name + " in view " + viewName + " for document " +
											owningModule.getName() + '.' + owningDocument.getName() +
											" for uxui " + uxui + " does not reference a valid view " +
											((name == null) ? viewName : name) + " in document " + m.getName() + '.' + d.getName());
		}
		
		ViewImpl view = (ViewImpl) UtilImpl.cloneBySerialization(originalView);
		// User Accesses are not required on the cloned view
		// NB may be null if access control is turned off
		Set<UserAccess> accesses = view.getAccesses();
		if (accesses != null) {
			accesses.clear();
		}

		ComponentViewVisitor visitor = new ComponentViewVisitor(customer, m, d, view, binding, names, widgetId);
		visitor.visit();
		contained = visitor.getContained();
	}
}
