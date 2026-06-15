package org.skyve.impl.report.jasperreports;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.user.User;

/**
 * JasperReports data source that exposes Skyve beans and bound values to report fields.
 */
public class SkyveDataSource implements JRDataSource {
	private User user;
	private Iterator<? extends Bean> iterator;
	private Bean currentBean;

	/**
	 * Creates a data source backed by a bean list.
	 *
	 * @param user The active reporting user.
	 * @param list Bean rows to expose to the report.
	 */
	public SkyveDataSource(User user, List<? extends Bean> list) {
		this(user, list.iterator());
	}

	/**
	 * Creates a data source backed by a bean iterator.
	 *
	 * @param user The active reporting user.
	 * @param iterator Bean iterator used for row traversal.
	 */
	public SkyveDataSource(User user, Iterator<? extends Bean> iterator) {
		this.user = user;
		this.iterator = iterator;
	}

	/**
	 * Creates a data source containing a single bean.
	 *
	 * @param user The active reporting user.
	 * @param bean The single bean row.
	 */
	public SkyveDataSource(User user, Bean bean) {
		this(user, createBeanList(bean));
	}
	
	/**
	 * Wraps a single bean in a list so it can be consumed by the iterator-based constructor.
	 *
	 * @param bean The bean to expose as the sole data-source row.
	 * @return A single-element bean list.
	 */
	private static List<? extends Bean> createBeanList(Bean bean) {
		List<Bean> result = new ArrayList<>();
		result.add(bean);
		return result;
	}
	
	/**
	 * Advances the data source cursor to the next bean row.
	 *
	 * @return {@code true} when a next row is available.
	 * @throws JRException Never thrown directly by this implementation, but part of
	 *         the {@link JRDataSource} contract.
	 */
	@Override
	public boolean next() 
	throws JRException {
		boolean hasNext = false;

		if (iterator != null) {
			hasNext = iterator.hasNext();
			if (hasNext) {
				currentBean = iterator.next();
			}
		}

		return hasNext;
	}

	/**
	 * Resolves a Jasper field value from the current Skyve bean row.
	 *
	 * <p>When a field description is present it is treated as a binding and a formatted
	 * display value is returned. Otherwise the field name is treated as the binding and
	 * the raw bound value is returned. Special bindings {@code THIS} and {@code USER}
	 * return the current bean and reporting user respectively.</p>
	 *
	 * @param field The Jasper field definition.
	 * @return The resolved field value.
	 * @throws JRException If binding resolution or formatting fails.
	 */
	@Override
	public Object getFieldValue(JRField field) 
	throws JRException {
		Object result = null;
		try {
			boolean formatted = true;
			String binding = field.getDescription();
			if ((binding == null) || binding.isEmpty()) {
				binding = field.getName();
				formatted = false;
			}
			if ("THIS".equals(binding)) {
				result = currentBean;
			}
			else if ("USER".equals(binding)) {
				result = user;
			}
			else {
				binding = BindUtil.unsanitiseBinding(binding);
				if (formatted) {
					result = BindUtil.getDisplay(user.getCustomer(), currentBean, binding);
				}
				else {
					result = BindUtil.get(currentBean, binding);
				}
			}
		}
		catch (Exception e) {
			throw new JRException("Could not populate field " + field.getName() + " from bizhub data source.", e);
		}

		return result;
	}

	/**
	 * Formats a value using Skyve display conversion rules for a binding.
	 *
	 * @param user The active reporting user.
	 * @param currentBean The current bean context.
	 * @param binding The binding expression.
	 * @param value The raw value to format.
	 * @return The formatted display value.
	 * @throws JRException If formatting fails.
	 */
	public static Object getFormattedValue(User user, Bean currentBean, String binding, Object value) throws JRException {
		try {
			return BindUtil.getDisplay(user.getCustomer(), currentBean, binding, value);
		}
		catch (Exception e) {
			throw new JRException(String.format("Could not format value %s for binding %s.", value, binding), e);
		}
	}
}
