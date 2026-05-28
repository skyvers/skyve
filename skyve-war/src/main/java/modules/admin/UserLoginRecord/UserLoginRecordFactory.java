package modules.admin.UserLoginRecord;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserLoginRecord;

@SkyveFactory(excludedUpdateAttributes = {UserLoginRecord.countryCodePropertyName, UserLoginRecord.countryNamePropertyName})
/**
 * Provides fixture defaults for {@code UserLoginRecord} document tests.
 */
public class UserLoginRecordFactory {
	// nothing to see here
}
