package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.metadata.DecoratedMetaData;

/**
 * Mixin interface for membership selection widgets ({@link CheckMembership},
 * {@link ListMembership}).
 *
 * <p>Declares the common contract for widgets that allow selecting members from
 * a candidate set, including the candidate binding and display attributes.
 */
public interface MembershipWidget extends DecoratedMetaData, Changeable {
	// no extra properties or methods
}
