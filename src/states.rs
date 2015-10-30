/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[macro_export]
macro_rules! state_pseudo_classes {
    ($macro_name: ident) => { $macro_name! {
        #[doc = "The mouse is down on this element. \
                 (https://html.spec.whatwg.org/multipage/#selector-active). \
                 FIXME(#7333): set/unset this when appropriate"]
        state "active" => Active / get_active_state / IN_ACTIVE_STATE = 0x01,
        #[doc = "This element has focus.
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-focus"]
        state "focus" => Focus / get_focus_state / IN_FOCUS_STATE = 0x02,
        #[doc = "The mouse is hovering over this element. \
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-hover"]
        state "hover" => Hover / get_hover_state / IN_HOVER_STATE = 0x04,
        #[doc = "Content is enabled (and can be disabled). \
                 http://www.whatwg.org/html/#selector-enabled"]
        state "enabled" => Enabled / get_enabled_state / IN_ENABLED_STATE = 0x08,
        #[doc = "Content is disabled. \
                 http://www.whatwg.org/html/#selector-disabled"]
        state "disabled" => Disabled / get_disabled_state / IN_DISABLED_STATE = 0x10,
        #[doc = "Content is checked. \
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-checked"]
        state "checked" => Checked / get_checked_state / IN_CHECKED_STATE = 0x20,
        #[doc = "https://html.spec.whatwg.org/multipage/scripting.html#selector-indeterminate"]
        state "indeterminate" => Indeterminate / get_intermediate_state / IN_INDETERMINATE_STATE = 0x40,
    }}
}

macro_rules! states_bitflag {
    ($(
        $(#[$Flag_attr: meta])*
        state $css: expr => $variant: ident / $method: ident /
        $flag: ident = $value: expr,
    )+) => {
        bitflags! {
            #[doc = "Event-based element states."]
            flags ElementState: u8 {
                $($(#[$Flag_attr])* const $flag = $value,)+
            }
        }
    }
}


state_pseudo_classes!(states_bitflag);
