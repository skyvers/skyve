package org.skyve.impl.bizport;

import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.model.Attribute;

import jakarta.annotation.Nullable;

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
	public enum LoadAction {
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
	

	public @Nullable Attribute getAttribute() {
		return attribute;
	}

	public void setAttribute(@Nullable Attribute attribute) {
		this.attribute = attribute;
	}

	public @Nullable Converter<?> getConverter() {
		return converter;
	}

	public void setConverter(@Nullable Converter<?> converter) {
		this.converter = converter;
	}

	public @Nullable String getBinding() {
		return binding;
	}

	public void setBinding(@Nullable String binding) {
		this.binding = binding;
	}

	public boolean isTreatEmptyNumericAsZero() {
		return treatEmptyNumericAsZero;
	}

	public void setTreatEmptyNumericAsZero(boolean treatEmptyNumericAsZero) {
		this.treatEmptyNumericAsZero = treatEmptyNumericAsZero;
	}

	public @Nullable LoadAction getLoadAction() {
		return loadAction;
	}
	
	public void setLoadAction(@Nullable LoadAction loadAction){
		this.loadAction = loadAction;
	}

	public boolean isRequired() {
		return required;
	}
	
	public @Nullable Integer getIndex() {
		return index;
	}

	public void setIndex(@Nullable Integer index) {
		this.index = index;
	}

	public void setIndex(int index) {
		this.index = Integer.valueOf(index);
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	//default constructor
	public DataFileField(@Nullable String binding) {
		this.binding = binding;
		this.loadAction = LoadAction.SET_VALUE;
		this.required=  false;
	}
	
	public DataFileField(@Nullable String binding, int index){
		this.binding = binding;
		this.loadAction = LoadAction.SET_VALUE;
		this.required=  false;
		this.index = Integer.valueOf(index);
	}
	
	public DataFileField(@Nullable String binding, @Nullable LoadAction loadAction, boolean required, int index) {
		this.binding = binding;
		this.loadAction = loadAction;
		this.required = required;
		this.index = Integer.valueOf(index);
	}

	public DataFileField(@Nullable String binding, @Nullable LoadAction loadAction, boolean required, int index, @Nullable Converter<?> converter) {
		this.binding = binding;
		this.loadAction = loadAction;
		this.required = required;
		this.index = Integer.valueOf(index);
		this.converter = converter;
	}
}
