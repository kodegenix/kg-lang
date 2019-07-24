pub const SCRIPT_TEMPLATE: &'static str =
r#"
(function(__){
<%STRICT_MODE%>

<%GLOBALS%>

function $Parser() {
    this.stack = [];

    this.actions = {
    <%ACTIONS%>
    };

    this.commands = {
    <%COMMANDS%>
    };

    this.modes = {
    <%MODES%>
    };

    this.channels = {
    <%CHANNELS%>
    };

    this.shift = function (token) {
        this.stack.push(token);
    };

    this.reduce = function (rule, n) {
        var args = this.stack.splice(this.stack.length - n, n);
        this.stack.push(this.actions[rule].apply(this, args));
    };

    this.accept = function () {
        this.stack.pop();
        this.$result = this.stack.pop();
        return this.$result;
    };

    this.clear = function () {
        this.stack = [];
        this.$result = null;
    };

    this.command = function (cmd, token) {
        this.commands[cmd].call(this, token);
        return token;
    };

    this.mode = function (mode) {
        return this.modes[mode];
    }

    this.channel = function (channel) {
        return this.channels[channel];
    }

    this.set_mode = function (mode) {
        if (typeof(mode) === 'string') {
            mode = this.mode(mode);
        }
        this.__set_mode(mode.index);
    };

    this.push_mode = function (mode) {
        if (typeof(mode) === 'string') {
            mode = this.mode(mode);
        }
        this.__push_mode(mode.index);
    };

    this.pop_mode = function () {
        this.__pop_mode();
    };

    this.set_channel = function (channel) {
        if (typeof(channel) === 'string') {
            channel = this.channel(channel);
        }
        this.__set_channel(channel.index);
    };

    this.push_channel = function (channel) {
        if (typeof(channel) === 'string') {
            channel = this.channel(channel);
        }
        this.__push_channel(channel.index);
    };

    this.pop_channel = function () {
        this.__pop_channel();
    };

    this.enqueue_token = function (token) {
        this.__enqueue_token(token);
    }
}

__.parser = new $Parser();

}(this));
"#;
