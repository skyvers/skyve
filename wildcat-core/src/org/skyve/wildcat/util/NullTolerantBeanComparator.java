package org.skyve.wildcat.util;

import org.apache.commons.beanutils.BeanComparator;
import org.skyve.wildcat.bind.BindUtil;

public class NullTolerantBeanComparator extends BeanComparator {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 6217533183265925971L;

	/**
	 * Used for sorting collections.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static final NullTolerantComparator<? extends Comparable<?>> NULL_TOLERANT_COMPARATOR = new NullTolerantComparator();

	public NullTolerantBeanComparator(String property) {
		super(property, NULL_TOLERANT_COMPARATOR);
	}

	@Override
	public int compare(Object o1, Object o2) {
		String property = getProperty();
		if (property == null) {
			return getComparator().compare(o1, o2);
		}

		try {
			Object value1 = BindUtil.get(o1, property);
	        Object value2 = BindUtil.get(o2, property);
	        return getComparator().compare(value1, value2);
		}
		catch (Exception e) {
			throw new RuntimeException("Cant compare " + o1 + " and " + o2, e);
		}
	}
}
