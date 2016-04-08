-type match() :: {ofp_field_type(), Value :: bitstring()}
               | {ofp_field_type(), Value :: bitstring(),
                  Mask :: bitstring()}.

-type packet_in() :: list(packet_in_property()).

-type packet_in_property() :: {buffer_id,  ofp_buffer_id()} |
                              {reason, ofp_packet_in_reason()} |
                              {table_id, non_neg_integer()} |
                              {cookie, binary()} | 
                              {match, match()} |
                              {data, binary()}.
