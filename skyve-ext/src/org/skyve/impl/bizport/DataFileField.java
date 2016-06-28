package org.skyve.impl.bizport;

/**
 * FieldLoader is a definition of how a data field will be loaded 
 * 
 * For example, a FieldLoader defines what binding corresponds to the data field 
 * and if the binding is a reference, how to use the data value to find the reference
 * to set the binding to.
 * 
 * @author Robert
 *
 */
public class DataFileField {
	
	public static enum LoadAction {
		SET_VALUE, LOOKUP_EQUALS, LOOKUP_LIKE, LOOKUP_CONTAINS, CONFIRM_VALUE
	}
	
	// the index of this field (e.g. the column number)
	private int index;
	
	//the binding to load to
	private String binding;
	
	//whether a value is required
	private boolean required = false;
	
	//the action to do with the value
	private LoadAction loadAction;
	
	//whether to treat a numeric value as 0
	private boolean treatEmptyNumericAsZero = false;
	
	public String getBinding() {
		return binding;
	}

	public void setBinding(String binding) {
		this.binding = binding;
	}

	public boolean isTreatEmptyNumericAsZero() {
		return treatEmptyNumericAsZero;
	}

	public void setTreatEmptyNumericAsZero(boolean treatEmptyNumericAsZero) {
		this.treatEmptyNumericAsZero = treatEmptyNumericAsZero;
	}

	public LoadAction getLoadAction() {
		return loadAction;
	}
	
	public void setLoadAction(LoadAction loadAction){
		this.loadAction = loadAction;
	}

	public boolean isRequired() {
		return required;
	}
	
	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	//default constructor
	public DataFileField(String binding){
		this.binding = binding;
		this.loadAction = LoadAction.SET_VALUE;
		this.required=  false;
	}
	
	public DataFileField(String binding, LoadAction loadAction, boolean required) {
		this.binding = binding;
		this.loadAction = loadAction;
		this.required = required;
	}
}
