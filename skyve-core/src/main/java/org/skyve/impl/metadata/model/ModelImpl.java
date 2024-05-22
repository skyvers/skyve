package org.skyve.impl.metadata.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.AbstractMetaDataMap;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Model;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public abstract class ModelImpl extends AbstractMetaDataMap implements Model {
	private static final long serialVersionUID = -9075615768687125545L;

	private String owningModuleName;

	/**
	 * The list of interfaces for this model in the order they are added.
	 */
	private List<Interface> interfaces = new ArrayList<>();
	/**
	 * The list of attributes for this model in the order they are added.
	 */
	private List<Attribute> attributes = new ArrayList<>();
	private String singularAlias;
	private Persistent persistent;
	private Dynamic dynamic;
	private Boolean hasDynamic = null;
	private Extends inherits;
	private boolean abstractClass;
	private String pluralAlias;
	private String icon16x16RelativeFileName;
	private String icon32x32RelativeFileName;
	private String iconStyleClass;
	private boolean audited = true;
	private String name;
	private String description;

	@Override
	public @Nonnull String getOwningModuleName() {
		return owningModuleName;
	}

	public void setOwningModuleName(String owningModuleName) {
		this.owningModuleName = owningModuleName;
	}

	@Override
	public @Nullable Attribute getAttribute(@SuppressWarnings("hiding") @Nonnull String name) {
		return (Attribute) getMetaData(name);
	}

	public void putAttribute(Attribute attribute) {
		putMetaData(attribute.getName(), attribute);
		attributes.add(attribute);
	}

	public void putInterface(Interface i) {
		interfaces.add(i);
	}

	@Override
	public List<Interface> getInterfaces() {
		return Collections.unmodifiableList(interfaces);
	}

	@Override
	public @Nonnull List<? extends Attribute> getAttributes() {
		return Collections.unmodifiableList(attributes);
	}
	
	@Override
	public @Nonnull List<? extends Attribute> getAllAttributes(@Nonnull Customer customer) {
		List<Attribute> result = new ArrayList<>(attributes);
		Extends currentInherits = inherits;
		if (currentInherits != null) {
			while (currentInherits != null) {
				Module module = customer.getModule(getOwningModuleName());
				Document baseDocument = module.getDocument(customer, currentInherits.getDocumentName());
				result.addAll(baseDocument.getAttributes());
				currentInherits = baseDocument.getExtends();
			}
		}
		
		return Collections.unmodifiableList(result);
	}

	@Override
	public @Nullable Attribute getPolymorphicAttribute(@Nonnull Customer customer, @SuppressWarnings("hiding") @Nonnull String name) {
		Attribute result = getAttribute(name);
		if (result == null) {
			Extends currentInherits = inherits;
			if (currentInherits != null) {
				while ((result == null) && (currentInherits != null)) {
					Module module = customer.getModule(getOwningModuleName());
					Document baseDocument = module.getDocument(customer, currentInherits.getDocumentName());
					result = baseDocument.getAttribute(name);
					currentInherits = baseDocument.getExtends();
				}
			}
		}
		return result;
	}
	
	@Override
	public String getPluralAlias() {
		return pluralAlias;
	}

	public void setPluralAlias(String pluralAlias) {
		this.pluralAlias = pluralAlias;
	}

	@Override
	public String getSingularAlias() {
		return singularAlias;
	}

	public void setSingularAlias(String singularAlias) {
		this.singularAlias = singularAlias;
	}

	@Override
	public String getIcon16x16RelativeFileName() {
		return icon16x16RelativeFileName;
	}

	public void setIcon16x16RelativeFileName(String icon16x16RelativeFileName) {
		this.icon16x16RelativeFileName = icon16x16RelativeFileName;
	}

	@Override
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = iconStyleClass;
	}

	@Override
	public String getIcon32x32RelativeFileName() {
		return icon32x32RelativeFileName;
	}

	public void setIcon32x32RelativeFileName(String icon32x32RelativeFileName) {
		this.icon32x32RelativeFileName = icon32x32RelativeFileName;
	}

	@Override
	public boolean isDynamic() {
		return (dynamic != null);
	}

	@Override
	public Dynamic getDynamism() {
		return dynamic;
	}
	
	public void setDynamism(Dynamic dynamic) {
		this.dynamic = dynamic;
	}
	
	@Override
	public boolean hasDynamic() {
		if (hasDynamic == null) {
			determineHasDynamic(new TreeSet<>());
		}

		return hasDynamic.booleanValue();
	}
	
	public void clearHasDynamic() {
		hasDynamic = null;
	}
	
	private void determineHasDynamic(Set<String> modoc) {
		Customer c = CORE.getCustomer();
		String omn = getOwningModuleName();
		modoc.add(omn + "." + getName());

		if (isDynamic()) {
			hasDynamic = Boolean.TRUE;
			return;
		}

		Module m = c.getModule(omn);
		for (Attribute a : getAllAttributes(c)) {
			if (BindUtil.isDynamic(c, m, a)) {
				hasDynamic = Boolean.TRUE;
				return;
			}
			if (a instanceof Relation) {
				String dn = ((Relation) a).getDocumentName();
				ModelImpl rd = (ModelImpl) m.getDocument(c, dn);
				if (modoc.add(rd.getOwningModuleName() + "." + dn)) {
					if (rd.hasDynamic == null) {
						rd.determineHasDynamic(modoc);
					}
					if (rd.hasDynamic.booleanValue()) {
						hasDynamic = Boolean.TRUE;
						return;
					}
				}
			}
		}

		hasDynamic = Boolean.FALSE;
	}
	
	@Override
	public boolean isAudited() {
		return audited;
	}

	public void setAudited(boolean audited) {
		this.audited = audited;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public Persistent getPersistent() {
		return persistent;
	}

	public void setPersistent(Persistent persistent) {
		this.persistent = persistent;
	}

	@Override
	public Extends getExtends() {
		return inherits;
	}

	public void setExtends(Extends inherits) {
		this.inherits = inherits;
	}

	@Override
	public boolean isAbstract() {
		return abstractClass;
	}

	public void setAbstract(boolean abstractClass) {
		this.abstractClass = abstractClass;
	}

    @Override
    public String toString() {

        return MoreObjects.toStringHelper(this)
                          .add("name", name)
                          .toString();
    }
}
