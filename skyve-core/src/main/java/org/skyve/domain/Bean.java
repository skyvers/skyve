package org.skyve.domain;

import java.io.Serializable;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * The root interface for all Skyve domain objects.
 *
 * <p>Every document instance — whether persistent, transient, hierarchical, or dynamic —
 * implements {@code Bean}. It carries the standard framework identity and ownership
 * properties (the {@code biz*} fields), change-tracking state, and dynamic attribute
 * support.
 *
 * <p>The {@code biz*} constants defined here are the canonical binding names used
 * throughout the framework (EL expressions, queries, view bindings, SAIL tests).
 * Callers should reference these constants rather than hard-coding the string literals.
 *
 * <p>Threading: {@code Bean} instances are <em>not</em> thread-safe. A bean obtained
 * from the current thread's {@link org.skyve.persistence.Persistence} must not be shared
 * with other threads.
 *
 * @see org.skyve.domain.PersistentBean
 * @see org.skyve.domain.TransientBean
 * @see org.skyve.domain.ChildBean
 * @see org.skyve.domain.HierarchicalBean
 * @see org.skyve.impl.domain.AbstractBean
 */
public interface Bean extends Serializable, Comparable<Bean> {
	/**
	 * Binding name for the bean's unique identifier ({@code bizId}).
	 * Corresponds to the primary key column in the database for persistent beans.
	 */
	public static final String DOCUMENT_ID = "bizId";

	/**
	 * Binding name for the owning customer name ({@code bizCustomer}).
	 * Used in multi-tenant deployments to scope rows to a specific customer.
	 */
	public static final String CUSTOMER_NAME = "bizCustomer";

	/**
	 * Binding name for the data-group identifier ({@code bizDataGroupId}).
	 * When non-{@code null}, this bean is visible only within the specified data group,
	 * enforcing row-level security partitions within a customer.
	 */
	public static final String DATA_GROUP_ID = "bizDataGroupId";

	/**
	 * Binding name for the owning user identifier ({@code bizUserId}).
	 * Identifies which user created or owns this bean for permission purposes.
	 */
	public static final String USER_ID = "bizUserId";

	/**
	 * Binding name for the ordinal position ({@code bizOrdinal}).
	 * Used by {@link ChildBean} to record the display/sort order within the parent's collection.
	 */
	public static final String ORDINAL_NAME = "bizOrdinal";
	
	/**
	 * Binding name for the module name ({@code bizModule}).
	 * Identifies which Skyve module this bean's document belongs to.
	 */
	public static final String MODULE_KEY = "bizModule";
	
	/**
	 * Binding name for the document name ({@code bizDocument}).
	 * Identifies the Skyve document (entity type) of this bean.
	 */
	public static final String DOCUMENT_KEY = "bizDocument";

	/**
	 * Binding name for the human-readable key ({@code bizKey}).
	 * Used as the display label for this bean in association pickers, list views,
	 * and audit trails. Set by the document's {@code bizKey} expression or the
	 * {@link org.skyve.metadata.model.document.Bizlet#getBizKey} method.
	 */
	public static final String BIZ_KEY = "bizKey";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean has been modified
	 * since it was loaded or created ({@code changed}).
	 * Usable in view conditions and SAIL test assertions.
	 */
	public static final String CHANGED_KEY = "changed";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean has <em>not</em>
	 * been modified ({@code notChanged}).
	 */
	public static final String NOT_CHANGED_KEY = "notChanged";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean exists in the database
	 * ({@code persisted}).
	 */
	public static final String PERSISTED_KEY = "persisted";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean does <em>not</em>
	 * exist in the database ({@code notPersisted}).
	 */
	public static final String NOT_PERSISTED_KEY = "notPersisted";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean is in creation mode,
	 * i.e. the {@code create.xml} view should be shown rather than the {@code edit.xml}
	 * view ({@code created}).
	 * Defaults to {@code true} for new instances.
	 */
	public static final String CREATED_KEY = "created";
	
	/**
	 * Condition name that evaluates to {@code true} when the bean is <em>not</em> in
	 * creation mode ({@code notCreated}).
	 */
	public static final String NOT_CREATED_KEY = "notCreated";

	/**
	 * Binding suffix for navigating to the {@code bizId} of a related bean via an
	 * association binding (e.g. {@code "myAssociation.bizId"}).
	 */
	public static final String DOCUMENT_ID_SUFFIX = ".bizId";

	/**
	 * Returns this bean's unique identifier.
	 *
	 * <p>For persistent beans this is a UUID string stored as the primary key.
	 * For transient and dynamic beans it is assigned when the instance is created.
	 * May be {@code null} for newly constructed instances that have never been
	 * initialised by the framework.
	 *
	 * @return the bean's unique identifier, or {@code null} for uninitialised instances
	 */
	public String getBizId();

	/**
	 * Returns the module name this bean belongs to.
	 *
	 * <p>Use {@link #getModuleMetaData()} to obtain the full {@link Module} object.
	 *
	 * @return the module name; never {@code null} for properly initialised beans
	 */
	public String getBizModule();

	/**
	 * Returns the {@link Module} metadata for this bean in the context of the current customer.
	 *
	 * <p>Convenience default equivalent to {@code CORE.getCustomer().getModule(getBizModule())}.
	 *
	 * @return the module metadata; never {@code null}
	 */
	public default Module getModuleMetaData() {
		return CORE.getCustomer().getModule(getBizModule());
	}
	
	/**
	 * Returns the document name this bean belongs to.
	 *
	 * <p>Use {@link #getDocumentMetaData()} to obtain the full {@link Document} object.
	 *
	 * @return the document name; never {@code null} for properly initialised beans
	 */
	public String getBizDocument();

	/**
	 * Returns the {@link Document} metadata for this bean in the context of the current customer.
	 *
	 * <p>Convenience default equivalent to
	 * {@code CORE.getCustomer().getModule(getBizModule()).getDocument(customer, getBizDocument())}.
	 *
	 * @return the document metadata; never {@code null}
	 */
	public default Document getDocumentMetaData() {
		Customer customer = CORE.getCustomer();
		return customer.getModule(getBizModule()).getDocument(customer, getBizDocument());
	}
	
	/**
	 * Returns the customer name this bean is scoped to.
	 *
	 * @return the customer name, or {@code null} if not yet set
	 */
	public String getBizCustomer();

	/**
	 * Sets the customer name this bean is scoped to.
	 *
	 * @param bizCustomer the customer name
	 */
	public void setBizCustomer(String bizCustomer);

	/**
	 * Returns the data-group identifier for row-level security partitioning.
	 *
	 * @return the data-group ID, or {@code null} if this bean is not scoped to a data group
	 */
	public String getBizDataGroupId();

	/**
	 * Sets the data-group identifier for row-level security partitioning.
	 *
	 * @param bizDataGroupId the data-group ID, or {@code null} to remove data-group scoping
	 */
	public void setBizDataGroupId(String bizDataGroupId);

	/**
	 * Returns the identifier of the user who owns this bean.
	 *
	 * @return the user ID, or {@code null} if not yet assigned
	 */
	public String getBizUserId();

	/**
	 * Sets the identifier of the user who owns this bean.
	 *
	 * @param bizUserId the user ID
	 */
	public void setBizUserId(String bizUserId);

	/**
	 * Returns the human-readable display key for this bean.
	 *
	 * <p>The biz key is computed from the document's {@code bizKey} expression (or
	 * overridden in the {@link org.skyve.metadata.model.document.Bizlet}) and is used as
	 * the display label in pickers and list views. It should be concise and descriptive.
	 *
	 * @return the display key; may be {@code null} for newly created instances
	 */
	public String getBizKey();
	
	/**
	 * Evaluates a named condition defined on this bean's document metadata.
	 *
	 * <p>Conditions are declared in the document XML as boolean expressions over the
	 * bean's attributes. The framework evaluates them using BeanShell and caches the
	 * result in the bean. Condition names starting with {@code "not"} are automatically
	 * negated (e.g. {@code "notActive"} evaluates the inverse of the {@code "active"}
	 * condition).
	 *
	 * @param conditionName the unqualified condition name as declared in document metadata; must not be {@code null}
	 * @return {@code true} if the condition is satisfied, {@code false} otherwise
	 */
	public boolean evaluateCondition(String conditionName);

	/**
	 * Returns a map of property names to their original (pre-modification) values.
	 *
	 * <p>Only properties that have been changed since the bean was loaded or created are
	 * present in the map. If a property has never been changed, it is absent.
	 *
	 * <p>This method intentionally does not follow the JavaBeans naming convention so
	 * that binding infrastructure does not treat it as a navigable property. All values
	 * must be {@link java.io.Serializable} because {@code Bean} is {@code Serializable}.
	 *
	 * @return a mutable map of property name to original value; never {@code null},
	 *         but may be empty if nothing has changed
	 */
	public Map<String, Serializable> originalValues();
	
	/**
	 * Returns {@code true} if any property on this bean has been modified.
	 *
	 * <p>A bean is considered changed when a setter is called with a value that is not
	 * equal to the previous value. This flag is set by the framework's change-tracking
	 * mechanism in {@link org.skyve.impl.domain.AbstractBean}.
	 *
	 * @return {@code true} if at least one property has been modified
	 * @see #hasChanged()
	 * @see #originalValues()
	 */
	public boolean isChanged();
	
	/**
	 * Returns {@code true} if no property on this bean has been modified.
	 *
	 * @return {@code true} if the bean is unmodified; equivalent to {@code !isChanged()}
	 */
	public boolean isNotChanged();

	/**
	 * Returns {@code true} if this bean or any bean reachable through its relations has been modified.
	 *
	 * <p>This method walks the entire object graph (associations, collections, nested beans)
	 * calling {@link #isChanged()} on each reachable bean. It is potentially expensive for
	 * deep or wide graphs; avoid calling it in tight loops.
	 *
	 * <p>Complexity: O(n) where n is the number of beans in the reachable object graph.
	 *
	 * @return {@code true} if this bean or any reachable related bean has been modified
	 * @see #isChanged()
	 */
	public boolean hasChanged();
	
	/**
	 * Returns {@code true} if this bean has been saved to the database.
	 *
	 * <p>For persistent beans this delegates to the persistence context's identity map.
	 * For transient beans this always returns {@code false}.
	 *
	 * @return {@code true} if a database row exists for this bean
	 */
	public boolean isPersisted();

	/**
	 * Returns {@code true} if this bean has <em>not</em> been saved to the database.
	 *
	 * @return {@code true} if no database row exists; equivalent to {@code !isPersisted()}
	 */
	public boolean isNotPersisted();

	/**
	 * Returns {@code true} if this bean is in creation mode.
	 *
	 * <p>Creation mode determines whether the framework renders the {@code create.xml}
	 * view or the {@code edit.xml} view. This is {@code true} for new instances until
	 * the bean is saved for the first time.
	 *
	 * @return {@code true} if the create view should be shown; defaults to {@code true}
	 *         for new instances
	 */
	public boolean isCreated();

	/**
	 * Returns {@code true} if this bean is <em>not</em> in creation mode.
	 *
	 * @return {@code true} if the edit view should be shown; equivalent to {@code !isCreated()}
	 */
	public boolean isNotCreated();
	
	/**
	 * Returns {@code true} if the named attribute is a dynamic (schema-less) attribute.
	 *
	 * <p>Dynamic attributes are not declared as typed fields on the generated class;
	 * instead they are stored in the bean's dynamic map and accessed via
	 * {@link #getDynamic}/{@link #setDynamic}. Use this method before calling
	 * {@code getDynamic} to avoid {@link IllegalArgumentException}.
	 *
	 * @param attributeName the simple (non-compound) attribute name to test; must not be {@code null}
	 * @return {@code true} if the attribute is dynamic
	 */
	public boolean isDynamic(String attributeName);

	/**
	 * Returns the value of a dynamic (schema-less) attribute.
	 *
	 * @param simpleBinding a non-compound binding expression (may be indexed or mapped); must not be {@code null}
	 * @return the dynamic attribute value, or {@code null} if the attribute has no value
	 * @throws IllegalArgumentException if {@code simpleBinding} does not refer to a known dynamic attribute
	 */
	public Object getDynamic(String simpleBinding);
	
	/**
	 * Sets the value of an existing dynamic (schema-less) attribute.
	 *
	 * @param simpleBinding a non-compound binding expression; must not be {@code null}
	 * @param value         the new value; may be {@code null}
	 * @throws IllegalArgumentException if {@code simpleBinding} does not refer to a known dynamic attribute
	 */
	public void setDynamic(String simpleBinding, Object value);

	/**
	 * Creates a new dynamic attribute with the given value if one does not already exist.
	 *
	 * <p>Unlike {@link #setDynamic}, this method is safe to call for attributes that may
	 * not yet have been registered; it will create the attribute if absent.
	 *
	 * @param simpleBinding a non-compound binding expression; must not be {@code null}
	 * @param value         the value to set; may be {@code null}
	 */
	public void putDynamic(String simpleBinding, Object value);

	/**
	 * Merges all entries from {@code dynamic} into this bean's dynamic attribute map,
	 * or resets it entirely if {@code dynamic} is {@code null}.
	 *
	 * <p>Existing dynamic attributes not present in {@code dynamic} are left unchanged.
	 * New attributes present in {@code dynamic} but not yet registered will be created.
	 * Passing {@code null} clears all dynamic attributes.
	 *
	 * @param dynamic the map of attribute names to values to merge, or {@code null} to
	 *                clear all dynamic attributes
	 */
	public void putAllDynamic(Map<String, Object> dynamic);
}
