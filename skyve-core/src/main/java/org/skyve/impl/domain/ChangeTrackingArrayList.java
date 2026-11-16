package org.skyve.impl.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * An ArrayList extension that places it's original state into it's owner bean's originalValues when a
 * method that mutates its state is invoked.
 * This class is used by domain generation when trackChanges is true on a collection,
 * whether it is transient or persistent.
 * Hibernate PersistentCollection replaces the instantiated collection for persistent collections and
 * AbstractBean.isChanged() checks these. 
 * 
 * NB Sort() may rearrange the elements but does not add/remove any elements.
 *		Since this method is called each time an ordered collection is submitted in a UI
 *		it has been left out from making the owning bean dirty (although strictly a reorder is a mutation)
 * 
 */
public class ChangeTrackingArrayList<E> extends ArrayList<E> {
	private static final long serialVersionUID = 476977428030134804L;

	private String propertyName; // used for placing in originalValues
	private AbstractBean owner;	// Used to call preset()
	
	public ChangeTrackingArrayList(@Nonnull String propertyName, @Nonnull AbstractBean owner) {
		this.propertyName = propertyName;
		this.owner = owner;
	}
	
	@Override
	public boolean add(@Nullable E e) {
		preset();
		return super.add(e);
	}
	
	@Override
	public void add(int index, @Nullable E element) {
		preset();
		super.add(index, element);
	}
	
	@Override
	public boolean addAll(@SuppressWarnings("null") @Nonnull Collection<? extends E> c) {
		preset();
		return super.addAll(c);
	}
	
	@Override
	public boolean addAll(int index, @SuppressWarnings("null") @Nonnull Collection<? extends E> c) {
		preset();
		return super.addAll(index, c);
	}
	
	@Override
	public @Nullable E remove(int index) {
		preset();
		return super.remove(index);
	}
	
	@Override
	public boolean remove(@Nullable Object o) {
		preset();
		return super.remove(o);
	}
	
	@Override
	public boolean removeAll(@SuppressWarnings("null") @Nonnull Collection<?> c) {
		preset();
		return super.removeAll(c);
	}
	
	@Override
	public boolean removeIf(@SuppressWarnings("null") @Nonnull Predicate<? super E> filter) {
		preset();
		return super.removeIf(filter);
	}
	
	@Override
	public void replaceAll(@SuppressWarnings("null") @Nonnull UnaryOperator<E> operator) {
		preset();
		super.replaceAll(operator);
	}
	
	@Override
	public boolean retainAll(@SuppressWarnings("null") @Nonnull Collection<?> c) {
		preset();
		return super.retainAll(c);
	}
	
	@Override
	public @Nullable E set(int index, @Nullable E element) {
		preset();
		return super.set(index, element);
	}

	private void preset() {
		Map<String, Serializable> originalValues = owner.originalValues();
		if (! originalValues.containsKey(propertyName)) {
			if (isEmpty()) {
				originalValues.put(propertyName, new ArrayList<>(0));
			}
			else {
				originalValues.put(propertyName, new ArrayList<>(this));
			}
		}
	}
}
