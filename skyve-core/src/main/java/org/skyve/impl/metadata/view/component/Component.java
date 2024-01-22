package org.skyve.impl.metadata.view.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.View;
import org.skyve.util.Binder.TargetMetaData;

/**
 * Implements a component.
 * A component can refer to another named view 
 * directly via a view name, a document name and view name, or a module name, document name and view name or
 * indirectly via a binding (can be compound)
 * A component can refer to a part of a view via a widgetId.
 * 
 * Note that equals and hash code are left as default - ie based on heap memory location.
 * This component is potentially used as a key in ComponentFragments (a concurrent WeakHashMap) on ViewImpl.
 * When the ViewImpl containing this component is evicted from the repository cache (in dev mode),
 * a new component is reloaded and the WeakHashMap entry in the component's target ViewImpl will be garbage collected.
 * If the target view is evicted from the repository cache, the ComponentFragments (ie the entire WeakHashMap) will go.
 *
 * @author mike
 */
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

	// target module/document/view that this component points at
	private String targetModule;
	private String targetDocument;
	private String targetView;
	
	public ViewImpl getFragment(CustomerImpl customer, String uxui) {
		ModuleImpl m = (ModuleImpl) customer.getModule(targetModule);
		DocumentImpl d = (DocumentImpl) m.getDocument(customer, targetDocument);
		ViewImpl v = (ViewImpl) d.getView(uxui, customer, targetView);

		// Ensure views are always a cloned fragment.
		// NB You might think that something like  
		// if ((getBinding() == null) && (widgetId == null) && names.isEmpty()) {
		//	return v;
		// }
		// would be good to reduce cloning overhead,
		// but consider where a component has a binding to a view that has a named component.
		// The named component will have the outer components binding prefix added to it by side-effect
		// unless a fragment for that particular usage is created.
		return v.getFragment(customer, m, d, uxui, this);
	}

	public void link(String uxui, CustomerImpl customer, ModuleImpl owningModule, DocumentImpl owningDocument, String viewName) {
		String binding = getBinding();
		ModuleImpl m = null;
		DocumentImpl d = null;
		if (binding != null) {
			try {
				TargetMetaData targetMetaData = BindUtil.getMetaDataForBinding(customer, owningModule, owningDocument, binding);
				Association a = (Association) targetMetaData.getAttribute();
				if (a == null) { // should never be
					throw new MetaDataException(binding + " doesn't point to an association");
				}
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
		
		targetModule = m.getName();
		targetDocument = d.getName();
		targetView = (name == null) ? viewName : name;
		
		View originalView = d.getView(uxui, customer, targetView);
		if (originalView == null) {
			throw new MetaDataException("Component named " + name + " in view " + viewName + " for document " +
											owningModule.getName() + '.' + owningDocument.getName() +
											" for uxui " + uxui + " does not reference a valid view " +
											((name == null) ? viewName : name) + " in document " + m.getName() + '.' + d.getName());
		}
	}
}
