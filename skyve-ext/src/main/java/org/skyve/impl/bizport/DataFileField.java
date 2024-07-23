package org.skyve.impl.bizport;

import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.model.Attribute;

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
	private Integer index;
	
	// the binding to load to
	private String binding;
	
	// whether a value is required
	private boolean required = false;
	
	// the action to do with the value
	private LoadAction loadAction;
	
	// whether to treat a numeric value as 0
	private boolean treatEmptyNumericAsZero = false;
	
	private Converter<?> converter;
	
	private Attribute attribute;
	

	public Attribute getAttribute() {
		return attribute;
	}

	public void setAttribute(Attribute attribute) {
		this.attribute = attribute;
	}

	public Converter<?> getConverter() {
		return converter;
	}

	public void setConverter(Converter<?> converter) {
		this.converter = converter;
	}

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
	
	public Integer getIndex() {
		return index;
	}

	public void setIndex(Integer index) {
		this.index = index;
	}

	public void setIndex(int index) {
		this.index = Integer.valueOf(index);
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	//default constructor
	public DataFileField(String binding) {
		this.binding = binding;
		this.loadAction = LoadAction.SET_VALUE;
		this.required=  false;
	}
	
	public DataFileField(String binding, int index){
		this.binding = binding;
		this.loadAction = LoadAction.SET_VALUE;
		this.required=  false;
		this.index = Integer.valueOf(index);
	}
	
	public DataFileField(String binding, LoadAction loadAction, boolean required, int index) {
		this.binding = binding;
		this.loadAction = loadAction;
		this.required = required;
		this.index = Integer.valueOf(index);
	}

	public DataFileField(String binding, LoadAction loadAction, boolean required, int index, Converter<?> converter) {
		this.binding = binding;
		this.loadAction = loadAction;
		this.required = required;
		this.index = Integer.valueOf(index);
		this.converter = converter;
	}
}
