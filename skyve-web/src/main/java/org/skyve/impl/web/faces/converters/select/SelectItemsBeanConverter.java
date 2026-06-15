package org.skyve.impl.web.faces.converters.select;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.web.faces.models.BeanMapAdapter;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;
import jakarta.faces.model.SelectItem;
import jakarta.faces.model.SelectItemGroup;

/**
 * Converts JSF values between formatted UI strings and Skyve domain representations for this format.
 */
public class SelectItemsBeanConverter implements Converter<Object> {
	/**
	 * Parses a UI string value into the corresponding domain value for this converter.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the submitted UI value
	 * @return the resolved domain value or identifier object
	 */
	@Override
	public Object getAsObject(FacesContext context, UIComponent component, String value) {
		Object result = null;

		if ((value != null) && (! value.isEmpty())) {
			result = findValueByStringConversion(context, component, value, this);
		}

		return result;
	}

	/**
	 * Formats a domain value as a UI string representation for this converter.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the domain value to convert
	 * @return the display or identifier string for the supplied value
	 */
	@Override
	public String getAsString(FacesContext context, UIComponent component, Object value) {
		String result = "";
		
		if (value instanceof BeanMapAdapter beanMapAdapter) {
			result = beanMapAdapter.getBean().getBizId();
		}
		else if (value instanceof Bean bean) {
			result = bean.getBizId();
		}
		else if (value instanceof Enumeration enumeration) {
			result = enumeration.toCode();
		}
		else if (value != null) {
			result = value.toString();
		}

		return result;
	}

	/**
	 * Searches all select items on the component for a value matching the supplied string.
	 *
	 * @param context the active JSF context
	 * @param component the component providing select items
	 * @param value the submitted UI value
	 * @param converter the converter used for string comparisons
	 * @return the matching item value, or {@code null} when no item matches
	 */
	private static Object findValueByStringConversion(FacesContext context, UIComponent component, String value, Converter<Object> converter) {
		return findValueByStringConversion(context, component, new SelectItemsIterator(context, component), value, converter);
	}

	/**
	 * Searches an iterator of select items for a value matching the supplied string.
	 *
	 * @param context the active JSF context
	 * @param component the component providing select items
	 * @param items the select items iterator to inspect
	 * @param value the submitted UI value
	 * @param converter the converter used for string comparisons
	 * @return the matching item value, or {@code null} when no item matches
	 */
	private static Object findValueByStringConversion(FacesContext context,
														UIComponent component,
														Iterator<SelectItem> items,
														String value,
														Converter<Object> converter) {
		while (items.hasNext()) {
			SelectItem item = items.next();
			Object itemValue = item.getValue();
			if (item instanceof SelectItemGroup selectItemGroup) {
				SelectItem[] subitems = selectItemGroup.getSelectItems();
				if ((subitems != null) && (subitems.length > 0)) {
					Object object = findValueByStringConversion(context,
																	component,
																	new ArrayIterator(subitems),
																	value,
																	converter);
					if (object != null) {
						return object;
					}
				}
			}
			else if ((! item.isNoSelectionOption()) && value.equals(converter.getAsString(context, component, itemValue))) {
				return itemValue;
			}
		}
		
		return null;
	}

	/**
	 * Returns {@code true} when the supplied array is null or has zero length.
	 *
	 * @param array the array to test
	 * @return {@code true} when null or empty, otherwise {@code false}
	 */
	public static boolean isEmpty(Object[] array) {
		return array == null || array.length == 0;
	}

	/**
	 * Iterates a fixed array of {@link SelectItem} values.
	 */
	static class ArrayIterator implements Iterator<SelectItem> {
		/**
		 * Creates an iterator over the supplied select-item array.
		 *
		 * @param items the backing array of select items
		 */
		public ArrayIterator(SelectItem[] items) {
			this.items = items;
		}

		private SelectItem[] items;
		private int index = 0;

		@Override
		public boolean hasNext() {
			return (index < items.length);
		}

		@Override
		public SelectItem next() {
			try {
				return (items[index++]);
			} catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
				throw new NoSuchElementException();
			}
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
}
