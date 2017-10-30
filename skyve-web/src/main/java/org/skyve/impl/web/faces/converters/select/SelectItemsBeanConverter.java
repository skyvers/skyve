package org.skyve.impl.web.faces.converters.select;

import java.util.Iterator;
import java.util.NoSuchElementException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.model.SelectItem;
import javax.faces.model.SelectItemGroup;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.web.faces.models.BeanMapAdapter;

public class SelectItemsBeanConverter implements Converter {
	@Override
	public Object getAsObject(FacesContext context, UIComponent component, String value) {
		Object result = null;

		if ((value != null) && (value.length() > 0)) {
			result = findValueByStringConversion(context, component, value, this);
		}

		return result;
	}

	@Override
	public String getAsString(FacesContext context, UIComponent component, Object value) {
		String result = "";
		
		if (value instanceof BeanMapAdapter) {
			result = ((BeanMapAdapter<?>) value).getBean().getBizId();
		}
		else if (value instanceof Bean) {
			result = ((Bean) value).getBizId();
		}
		else if (value instanceof Enumeration) {
			result = ((Enumeration) value).toCode();
		}
		else if (value != null) {
			result = value.toString();
		}

		return result;
	}

	public static Object findValueByStringConversion(FacesContext context, UIComponent component, String value, Converter converter) {
		return findValueByStringConversion(context, component, new SelectItemsIterator(context, component), value, converter);
	}

	private static Object findValueByStringConversion(FacesContext context,
														UIComponent component,
														Iterator<SelectItem> items,
														String value,
														Converter converter) {
		while (items.hasNext()) {
			SelectItem item = items.next();
			Object itemValue = item.getValue();
			if (item instanceof SelectItemGroup) {
				SelectItem subitems[] = ((SelectItemGroup) item).getSelectItems();
				if ((subitems != null) && (subitems.length > 0)) {
					Object object = findValueByStringConversion(context,
																	component,
																	new ArrayIterator(subitems), value,
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

	public static boolean isEmpty(Object[] array) {
		return array == null || array.length == 0;
	}

	/**
	 * This class is based on Mojarra version
	 */
	static class ArrayIterator implements Iterator<SelectItem> {
		public ArrayIterator(SelectItem items[]) {
			this.items = items;
		}

		private SelectItem items[];
		private int index = 0;

		@Override
		public boolean hasNext() {
			return (index < items.length);
		}

		@Override
		public SelectItem next() {
			try {
				return (items[index++]);
			} catch (IndexOutOfBoundsException e) {
				throw new NoSuchElementException();
			}
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
}
