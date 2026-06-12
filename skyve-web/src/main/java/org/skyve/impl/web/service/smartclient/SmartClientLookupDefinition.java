package org.skyve.impl.web.service.smartclient;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;

/**
 * SmartClient lookup definition encapsulates metadata used to render SmartClient option data sources and pick lists for lookup fields.
 * This includes the backing metadata query, display field, pick list fields, and filter fields.
 */
public class SmartClientLookupDefinition {
	private boolean bindingToDataGrid;
	// The data source for the drop down box
	private String optionDataSource;
    private String displayField;
    // List Grid fields for option data source
    private List<String> pickListFields = new ArrayList<>();
    // Filter fields for option data source
    private List<String> filterFields = new ArrayList<>();
    private MetaDataQueryDefinition query;
    private boolean canCreate = true;
    private boolean canUpdate = true;
    
	/**
	 * Creates lookup metadata used to render SmartClient option data sources and pick lists.
	 *
	 * @param bindingToDataGrid whether the lookup is bound to a data-grid context
	 * @param user active user, or {@code null}
	 * @param customer active customer metadata
	 * @param module module containing the relation/query metadata
	 * @param document document containing the lookup relation
	 * @param relation relation metadata for the lookup target
	 * @param lookup lookup widget metadata, or {@code null}
	 * @param runtime whether runtime domain/query values should be resolved
	 * @param uxui active UX/UI profile name
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	protected SmartClientLookupDefinition(boolean bindingToDataGrid,
		    								User user,
		    								Customer customer,
		    								Module module,
		    								Document document,
		    								Relation relation,
		    								LookupDescription lookup,
		    								boolean runtime,
		    								String uxui) {
        this.bindingToDataGrid = bindingToDataGrid;
        String queryName = (lookup == null) ? null : lookup.getQuery();
        // Use reference query name if none provided in lookup
        if ((queryName == null) && (relation instanceof Reference reference)) {
        	queryName = reference.getQueryName();
        }
		// Use the default query if none is defined, else get the named query.
        if (queryName == null) {
        	query = module.getDocumentDefaultQuery(customer, relation.getDocumentName());
        	queryName = query.getName();
        }
        else {
        	query = module.getNullSafeMetaDataQuery(queryName);
        }
        
        StringBuilder sb = new StringBuilder(128);
        sb.append(module.getName()).append('_').append(queryName).append('_');
        sb.append(document.getName()).append('_').append(relation.getName());
        optionDataSource = sb.toString();

        String descriptionBinding = (lookup == null) ? null : lookup.getDescriptionBinding();
        displayField = (descriptionBinding == null) ? 
        					Bean.BIZ_KEY : 
    						BindUtil.sanitiseBinding(descriptionBinding);

        Document queryDocument = module.getDocument(customer, query.getDocumentName());
        
        if (user != null) {
            canCreate = user.canCreateDocument(queryDocument);
            canUpdate = user.canUpdateDocument(queryDocument);
        }
        
        List<LookupDescriptionColumn> dropDownColumns = (lookup == null) ? null : lookup.getDropDownColumns();
        if ((dropDownColumns == null) || dropDownColumns.isEmpty()) {
        	pickListFields.add(displayField);
        }
        else {
            for (MetaDataQueryColumn column : query.getColumns()) {
            	String alias = column.getName();
            	if (alias == null) {
            		alias = column.getBinding();
            	}
            	final String a = alias;
            	Optional<LookupDescriptionColumn> optional = dropDownColumns.stream().filter(c -> a.equals(c.getName())).findAny();
            	if (optional.isPresent()) {
            		if ((column instanceof MetaDataQueryProjectedColumn projected) && projected.isProjected()) {
                        SmartClientQueryColumnDefinition def = SmartClientViewRenderer.getQueryColumn(user,
                    																					customer, 
																                                        module,
																                                        queryDocument,
																                                        column,
																                                        runtime,
																                                        uxui);

                    	pickListFields.add(def.getName());
                    	// only add fields that are filterable and can use the substring operator
                    	Boolean filterable = optional.get().getFilterable();
                    	if (Boolean.TRUE.equals(filterable)) {
                    		filterFields.add(def.getName());
                    	}
                    	else if ((filterable == null) && 
                    				def.isCanFilter() && 
                    				def.getHasTextFilterOperators()) {
                    		filterFields.add(def.getName());
                    	}
            		}
            	}
            }
        }
    }
    
	/**
	 * Returns the field name used to display selected lookup values.
	 *
	 * @return display field name
	 */
	public String getDisplayField() {
        return displayField;
    }

	/**
	 * Sets the field name used to display selected lookup values.
	 *
	 * @param displayField display field name
	 */
    public void setDisplayField(String displayField) {
        this.displayField = displayField;
    }

	/**
	 * Returns the SmartClient option data source identifier for this lookup.
	 *
	 * @return option data source identifier
	 */
    public String getOptionDataSource() {
        return optionDataSource;
    }

	/**
	 * Sets the SmartClient option data source identifier for this lookup.
	 *
	 * @param optionDataSource option data source identifier
	 */
    public void setOptionDataSource(String optionDataSource) {
        this.optionDataSource = optionDataSource;
    }

	/**
	 * Returns the configured pick-list fields for lookup selection.
	 *
	 * @return configured pick-list fields
	 */
    public List<String> getPickListFields() {
        return pickListFields;
    }

	/**
	 * Returns the fields that should participate in lookup filtering.
	 *
	 * @return lookup filter fields
	 */
    public List<String> getFilterFields() {
        return filterFields;
    }

	/**
	 * Returns the metadata query backing this lookup.
	 *
	 * @return backing metadata query
	 */
    public MetaDataQueryDefinition getQuery() {
        return query;
    }

	/**
	 * Sets the metadata query backing this lookup.
	 *
	 * @param query backing metadata query
	 */
    public void setQuery(MetaDataQueryDefinition query) {
        this.query = query;
    }

	/**
	 * Indicates whether this lookup is being rendered for a data-grid binding.
	 *
	 * @return {@code true} when rendered for a data-grid binding
	 */
	public boolean isBindingToDataGrid() {
		return bindingToDataGrid;
	}

	/**
	 * Sets whether this lookup is being rendered for a data-grid binding.
	 *
	 * @param bindingToDataGrid whether rendered for a data-grid binding
	 */
	public void setBindingToDataGrid(boolean bindingToDataGrid) {
		this.bindingToDataGrid = bindingToDataGrid;
	}

	/**
	 * Indicates whether create access is available for the lookup target document.
	 *
	 * @return {@code true} when create access is available
	 */
	public boolean getCanCreate() {
		return canCreate;
	}

	/**
	 * Sets whether create access is available for the lookup target document.
	 *
	 * @param canCreate whether create access is available
	 */
	public void setCanCreate(boolean canCreate) {
		this.canCreate = canCreate;
	}

	/**
	 * Indicates whether update access is available for the lookup target document.
	 *
	 * @return {@code true} when update access is available
	 */
	public boolean getCanUpdate() {
		return canUpdate;
	}

	/**
	 * Sets whether update access is available for the lookup target document.
	 *
	 * @param canUpdate whether update access is available
	 */
	public void setCanUpdate(boolean canUpdate) {
		this.canUpdate = canUpdate;
	}
}
