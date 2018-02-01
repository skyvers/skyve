package org.skyve.impl.jasperreports;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.user.User;

public class SkyveDataSource implements JRDataSource {
	private User user;
	private Iterator<? extends Bean> iterator;
	private Bean currentBean;

	public SkyveDataSource(User user, List<? extends Bean> list) {
		this(user, list.iterator());
	}

	public SkyveDataSource(User user, Iterator<? extends Bean> iterator) {
		this.user = user;
		this.iterator = iterator;
	}

	public SkyveDataSource(User user, Bean bean) {
		this(user, createBeanList(bean));
	}
	
	private static List<? extends Bean> createBeanList(Bean bean) {
		List<Bean> result = new ArrayList<>();
		result.add(bean);
		return result;
	}
	
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
	 * If field name is filled then we are after the domain value as a java object proper. 
	 * If field name is null and field description is filled then we are after the stringified and formatted version of the value.
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
}
