## Mirage channels

Channels are buffered reader/writers built on top of unbuffered `FLOW`
implementations.

Example:

```ocaml
module Channel = Channel.Make(Flow)
...
Channel.read_exactly ~len:16 t
>>= fun bufs -> (* read header of message *)
let payload_length = Cstruct.(LE.get_uint16 (concat bufs) 0) in
Channel.read_exactly ~len:payload_length t
>>= fun bufs -> (* payload of message *)

(* process message *)

Channel.write_buffer t header;
Channel.write_buffer t payload;
Channel.flush t
>>= fun () ->
```

