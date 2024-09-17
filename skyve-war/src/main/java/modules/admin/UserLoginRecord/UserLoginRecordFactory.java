package modules.admin.UserLoginRecord;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserLoginRecord;

@SkyveFactory(excludedUpdateAttributes = UserLoginRecord.countryPropertyName)
public class UserLoginRecordFactory {
	// nothing to see here
}
