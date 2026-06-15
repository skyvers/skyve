package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReferenceTarget.ReferenceTargetType;

@SuppressWarnings("static-method")
class LinkTest {

        @Test
        void showsLabelByDefaultReturnsFalse() {
                assertFalse(new Link().showsLabelByDefault());
        }

        @Test
        void jaxbHelperGetVisibleConditionNameReturnsNull() {
                assertNull(new Link().getVisibleConditionName());
        }

        @Test
        void getLocalisedValueWithNullValueReturnsNull() {
                assertNull(new Link().getLocalisedValue());
        }

        @Test
        void setVisibleConditionNameStoresNegatedCondition() {
                Link link = new Link();
                link.setVisibleConditionName("active");
                assertNotNull(link.getInvisibleConditionName());
        }

        @Test
        void getPropertiesReturnsNonNull() {
                assertNotNull(new Link().getProperties());
	}

        @Test
        void settersPreserveReferenceAndProcessValue() {
                Link link = new Link();
                ExternalReference reference = new ExternalReference();
                reference.setHref("  https://example.com  ");
                ReferenceTarget target = new ReferenceTarget();
                target.setType(ReferenceTargetType.blankFrame);
                target.setName("  linkTarget  ");

                link.setReference(reference);
                link.setTarget(target);
                link.setValue("  Link title  ");

                assertEquals(reference, link.getReference());
                assertEquals(target, link.getTarget());
                assertEquals("Link title", link.getValue());
        }
}
