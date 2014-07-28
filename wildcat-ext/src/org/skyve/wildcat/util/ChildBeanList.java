package org.skyve.wildcat.util;

import java.util.ArrayList;
import java.util.Collection;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;

public class ChildBeanList<T extends Bean> extends ArrayList<ChildBean<T>> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4567979821612682409L;

	private T parent;

	public ChildBeanList(T parent) {
		this.parent = parent;
	}

	public T getParent() {
		return parent;
	}

	@Override
	public boolean add(ChildBean<T> bean) {
		bean.setParent(parent);
		return super.add(bean);
	}

	@Override
	public void add(int index, ChildBean<T> element) {
		element.setParent(parent);
		super.add(index, element);
	}

	@Override
	public boolean addAll(Collection<? extends ChildBean<T>> c) {
		for (ChildBean<T> bean : c) {
			bean.setParent(parent);
		}

		return super.addAll(c);
	}

	@Override
	public boolean addAll(int index, Collection<? extends ChildBean<T>> c) {
		for (ChildBean<T> bean : c) {
			bean.setParent(parent);
		}

		return super.addAll(index, c);
	}

	@Override
	public void clear() {
		for (ChildBean<T> bean : this) {
			bean.setParent(null);
		}

		super.clear();
	}

	@Override
	public ChildBean<T> remove(int index) {
		get(index).setParent(null);
		return super.remove(index);
	}

	@Override
	@SuppressWarnings("unchecked")
	public boolean remove(Object o) {
		if (o instanceof ChildBean) {
			((ChildBean<T>) o).setParent(null);
		}
		return super.remove(o);
	}

	@Override
	@SuppressWarnings("unchecked")
	public boolean removeAll(Collection<?> c) {
		for (Object o : c) {
			if (o instanceof ChildBean) {
				((ChildBean<T>) o).setParent(null);
			}
		}

		return super.removeAll(c);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException("Can't be stuffed supporting this");
	}

	@Override
	public ChildBean<T> set(int index, ChildBean<T> element) {
		element.setParent(parent);
		return super.set(index, element);
	}
}
