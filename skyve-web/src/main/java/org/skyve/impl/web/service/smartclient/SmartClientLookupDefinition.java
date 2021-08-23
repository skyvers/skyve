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
    
	protected SmartClientLookupDefinition(boolean bindingToDataGrid,
		    								User user,
		    								Customer customer,
		    								Module module,
		    								Document document,
		    								Relation relation,
		    								LookupDescription lookup,
		    								boolean runtime) {
        this.bindingToDataGrid = bindingToDataGrid;
        String queryName = (lookup == null) ? null : lookup.getQuery();
        // Use reference query name if none provided in lookup
        if ((queryName == null) && (relation instanceof Reference)) {
        	queryName = ((Reference) relation).getQueryName();
        }
		// Use the default query if none is defined, else get the named query.
        if (queryName == null) {
        	query = module.getDocumentDefaultQuery(customer, relation.getDocumentName());
        	queryName = query.getName();
        }
        else {
        	query = module.getMetaDataQuery(queryName);
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
            		if ((column instanceof MetaDataQueryProjectedColumn) &&
            				((MetaDataQueryProjectedColumn) column).isProjected()) {
                        SmartClientQueryColumnDefinition def = SmartClientViewRenderer.getQueryColumn(user,
                    																					customer, 
																                                        module,
																                                        queryDocument,
																                                        column,
																                                        runtime);

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
    
	public String getDisplayField() {
        return displayField;
    }

    public void setDisplayField(String displayField) {
        this.displayField = displayField;
    }

    public String getOptionDataSource() {
        return optionDataSource;
    }

    public void setOptionDataSource(String optionDataSource) {
        this.optionDataSource = optionDataSource;
    }

    public List<String> getPickListFields() {
        return pickListFields;
    }

    public List<String> getFilterFields() {
        return filterFields;
    }

    public MetaDataQueryDefinition getQuery() {
        return query;
    }

    public void setQuery(MetaDataQueryDefinition query) {
        this.query = query;
    }

	public boolean isBindingToDataGrid() {
		return bindingToDataGrid;
	}

	public void setBindingToDataGrid(boolean bindingToDataGrid) {
		this.bindingToDataGrid = bindingToDataGrid;
	}

	public boolean getCanCreate() {
		return canCreate;
	}

	public void setCanCreate(boolean canCreate) {
		this.canCreate = canCreate;
	}

	public boolean getCanUpdate() {
		return canUpdate;
	}

	public void setCanUpdate(boolean canUpdate) {
		this.canUpdate = canUpdate;
	}
}
